#' @title Resample a Learner on a Task
#'
#' @description
#' Runs a resampling (possibly in parallel):
#' Repeatedly apply [Learner] `learner` on a training set of [Task] `task` to train a model,
#' then use the trained model to predict observations of a test set.
#' Training and test sets are defined by the [Resampling] `resampling`.
#'
#' @param task ([Task]).
#' @param learner ([Learner]).
#' @param resampling ([Resampling]).
#' @template param_store_models
#' @template param_store_backends
#' @template param_encapsulate
#' @template param_allow_hotstart
#' @template param_clone
#' @return [ResampleResult].
#'
#' @template section_predict_sets
#' @template section_parallelization
#' @template section_progress_bars
#' @template section_logging
#'
#' @note
#' The fitted models are discarded after the predictions have been computed in order to reduce memory consumption.
#' If you need access to the models for later analysis, set `store_models` to `TRUE`.
#'
#' @template seealso_resample
#' @export
#' @examples
#' task = tsk("penguins")
#' learner = lrn("classif.rpart")
#' resampling = rsmp("cv")
#'
#' # Explicitly instantiate the resampling for this task for reproduciblity
#' set.seed(123)
#' resampling$instantiate(task)
#'
#' rr = resample(task, learner, resampling)
#' print(rr)
#'
#' # Retrieve performance
#' rr$score(msr("classif.ce"))
#' rr$aggregate(msr("classif.ce"))
#'
#' # merged prediction objects of all resampling iterations
#' pred = rr$prediction()
#' pred$confusion
#'
#' # Repeat resampling with featureless learner
#' rr_featureless = resample(task, lrn("classif.featureless"), resampling)
#'
#' # Convert results to BenchmarkResult, then combine them
#' bmr1 = as_benchmark_result(rr)
#' bmr2 = as_benchmark_result(rr_featureless)
#' print(bmr1$combine(bmr2))
resample_rush = function(task, learner, resampling, store_models = FALSE, store_backends = TRUE, encapsulate = NA_character_, allow_hotstart = FALSE, clone = c("task", "learner", "resampling")) {
  assert_subset(clone, c("task", "learner", "resampling"))
  task = assert_task(as_task(task, clone = "task" %in% clone))
  learner = assert_learner(as_learner(learner, clone = "learner" %in% clone))
  resampling = assert_resampling(as_resampling(resampling, clone = "resampling" %in% clone))
  assert_flag(store_models)
  assert_flag(store_backends)
  assert_learnable(task, learner)

  set_encapsulation(list(learner), encapsulate)
  if (!resampling$is_instantiated) {
    resampling = resampling$instantiate(task)
  }
  n = resampling$iters
  pb = if (isNamespaceLoaded("progressr")) {
    # NB: the progress bar needs to be created in this env
    pb = progressr::progressor(steps = n)
  } else {
    NULL
  }
  lgr_threshold = map_int(mlr_reflections$loggers, "threshold")

  # initialize rush
  require_namespaces("rush")
  if (!rush::rush_available()) {
    stop("rush not available. Please set `rush_plan()`.")
  }
  rush = rsh()

  grid = if (allow_hotstart) {

    lg$debug("Resampling with hotstart enabled.")

    hotstart_grid = map_dtr(seq_len(n), function(iteration) {
      if (!is.null(learner$hotstart_stack)) {
        # search for hotstart learner
        task_hashes = task_hashes(task, resampling)
        start_learner = get_private(learner$hotstart_stack)$.start_learner(learner$clone(), task_hashes[iteration])
      }
      if (is.null(learner$hotstart_stack) || is.null(start_learner)) {
        # no hotstart learners stored or no adaptable model found
        lg$debug("Resampling with hotstarting not possible. Not start learner found.")
        mode = "train"
      } else {
        # hotstart learner found
        lg$debug("Resampling with hotstarting.")
        start_learner$param_set$values = insert_named(start_learner$param_set$values, learner$param_set$values)
        learner = start_learner
        mode = "hotstart"
      }
      data.table(learner = list(learner), mode = mode)
    })

    # null hotstart stack to reduce overhead in parallelization
    walk(hotstart_grid$learner, function(learner) {
      learner$hotstart_stack = NULL
      learner
    })
    hotstart_grid
  } else {
    data.table(learner = replicate(n, learner), mode = "train")
  }

  rush <<- rush

  rush$start_workers(
    fun = workhorse,
    packages = "mlr3",
    constants = list(task = task, resampling = resampling, lgr_threshold = lgr_threshold),
    await_workers = FALSE
  )

  grid[, iteration := seq_len(n)]
  keys = rush$push_tasks(transpose_list(grid))
  rush$await_tasks(keys)

  res = rush$read_hashes(keys, "ys")

  data = data.table(
    task = list(task),
    learner = grid$learner,
    learner_state = map(res, "learner_state"),
    resampling = list(resampling),
    iteration = seq_len(n),
    prediction = map(res, "prediction"),
    uhash = UUIDgenerate(),
    param_values = map(res, "param_values"),
    learner_hash = map_chr(res, "learner_hash")
  )

  rush$stop_workers(type = "kill")

  ResampleResult$new(ResultData$new(data, store_backends = store_backends))
}


workhorse_rush = function(rush, iteration, task, learner, resampling, param_values = NULL, lgr_threshold, store_models = FALSE, pb = NULL, mode = "train") {
  if (!is.null(pb)) {
    pb(sprintf("%s|%s|i:%i", task$id, learner$id, iteration))
  }

  # restore logger thresholds
  for (package in names(lgr_threshold)) {
    logger = lgr::get_logger(package)
    threshold = lgr_threshold[package]
    logger$set_threshold(threshold)
  }

  lg$info("%s learner '%s' on task '%s' (iter %i/%i)",
    if (mode == "train") "Applying" else "Hotstarting", learner$id, task$id, iteration, resampling$iters)

  sets = list(
    train = resampling$train_set(iteration),
    test = resampling$test_set(iteration),
    holdout = task$row_roles$holdout
  )

  # train model
  learner = learner$clone()
  if (length(param_values)) {
    learner$param_set$values = list()
    learner$param_set$set_values(.values = param_values)
  }
  learner_hash = learner$hash
  learner = learner_train(learner, task, sets[["train"]], sets[["test"]], mode = mode)

  # predict for each set
  sets = sets[learner$predict_sets]
  pdatas = Map(function(set, row_ids) {
    lg$debug("Creating Prediction for predict set '%s'", set)
    learner_predict(learner, task, row_ids)
  }, set = names(sets), row_ids = sets)
  pdatas = discard(pdatas, is.null)

  if (!store_models) {
    lg$debug("Erasing stored model for learner '%s'", learner$id)
    learner$state$model = NULL
  }

  list(learner_state = learner$state, prediction = pdatas, param_values = learner$param_set$values, learner_hash = learner_hash)
}

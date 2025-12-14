#' @title Resample a Learner on a Task
#'
#' @description
#' Runs a resampling (possibly in parallel):
#' Repeatedly apply [Learner] `learner` on a training set of [Task] `task` to train a model,
#' then use the trained model to predict observations of a test set.
#' Training and test sets are defined by the [Resampling] `resampling`.
#'
#' @section Stochasticity:
#' Note that uninstantiated [`Resampling`]s are instantiated on the task, making
#' the procedure stochastic even in case of a deterministic learner.
#'
#' @param task ([Task]).
#' @param learner ([Learner]).
#' @param resampling ([Resampling]).
#' @template param_store_models
#' @template param_store_backends
#' @template param_encapsulate
#' @template param_allow_hotstart
#' @template param_clone
#' @template param_unmarshal
#' @template param_callbacks
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
resample = function(
  task,
  learner,
  resampling,
  store_models = FALSE,
  store_backends = TRUE,
  encapsulate = NA_character_,
  allow_hotstart = FALSE,
  clone = c("task", "learner", "resampling"),
  unmarshal = TRUE,
  callbacks = NULL
) {

  lg$debug("Start resampling")

  assert_subset(clone, c("task", "learner", "resampling"))
  task = assert_task(as_task(task, clone = "task" %chin% clone))
  learner = assert_learner(as_learner(learner, clone = "learner" %chin% clone, discard_state = TRUE))
  resampling = assert_resampling(as_resampling(resampling, clone = "resampling" %chin% clone))
  assert_flag(store_models)
  assert_flag(store_backends)
  # this does not check the internal validation task as it might not be set yet
  assert_learnable(task, learner)
  assert_flag(unmarshal)
  callbacks = assert_callbacks(as_callbacks(callbacks))

  set_encapsulation(list(learner), encapsulate)
  if (!resampling$is_instantiated) {
    lg$info("Resampling '%s' is being instantiated on task '%s'", resampling$id, task$id)
    resampling = resampling$instantiate(task)
  }

  if (!is.null(resampling$task_row_hash) && resampling$task_row_hash != task$row_hash) {
    error_input("Resampling '%s' is not instantiated on task '%s'", resampling$id, task$id)
  }

  n = resampling$iters
  pb = if (isNamespaceLoaded("progressr")) {
    # NB: the progress bar needs to be created in this env
    pb = progressr::progressor(steps = n)
  } else {
    NULL
  }

  grid = if (allow_hotstart) {

    lg$debug("Resampling with hotstart enabled.")

    hotstart_grid = map_dtr(seq_len(n), function(iteration) {
      if (!is.null(learner$hotstart_stack)) {
        # search for hotstart learner
        task_hashes = resampling_task_hashes(task, resampling, learner)
        start_learner = get_private(learner$hotstart_stack)$.start_learner(learner$clone(), task_hashes[iteration])
      }
      if (is.null(learner$hotstart_stack) || is.null(start_learner)) {
        # no hotstart learners stored or no adaptable model found
        lg$debug("Resampling with hotstarting not possible. No start learner found.")
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

  res = future_map(n, workhorse, iteration = seq_len(n), learner = grid$learner, mode = grid$mode,
    MoreArgs = list(task = task, resampling = resampling, store_models = store_models, lgr_index = lgr::logger_index(), pb = pb, unmarshal = unmarshal, callbacks = callbacks)
  )

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

  data_extra = if (length(callbacks) && any(map_lgl(res, function(x) !is.null(x$data_extra)))) map(res, "data_extra")

  result_data = ResultData$new(data, data_extra, store_backends = store_backends)

  # the worker already ensures that models are sent back in marshaled form if unmarshal = FALSE, so we don't have
  # to do anything in this case. This allows us to minimize the amount of marshaling in those situtions where
  # the model is available in both states on the worker
  if (unmarshal && store_models) {
    result_data$unmarshal()
  }

  ResampleResult$new(result_data)
}

#' @title Benchmark Multiple Learners on Multiple Tasks
#'
#' @description
#' Runs a benchmark on arbitrary combinations of tasks ([Task]), learners ([Learner]), and resampling strategies ([Resampling]), possibly in parallel.
#'
#' For large-scale benchmarking we recommend to use the \CRANpkg{mlr3batchmark} package.
#' This package runs benchmark experiments on high-performance computing clusters and handles failed experiments.
#'
#' @param design ([data.frame()])\cr
#'   Data frame (or [data.table::data.table()]) with three columns: "task", "learner", and "resampling".
#'   Each row defines a resampling by providing a [Task], [Learner] and an instantiated [Resampling] strategy.
#'   The helper function [benchmark_grid()] can assist in generating an exhaustive design (see examples) and
#'   instantiate the [Resampling]s per [Task].
#'   Additionally, you can set the additional column 'param_values', see [benchmark_grid()].
#' @template param_store_models
#' @template param_store_backends
#' @template param_encapsulate
#' @template param_allow_hotstart
#' @template param_clone
#' @template param_unmarshal
#' @template param_callbacks
#'
#' @return [BenchmarkResult].
#'
#' @note
#' The fitted models are discarded after the predictions have been scored in order to reduce memory consumption.
#' If you need access to the models for later analysis, set `store_models` to `TRUE`.
#'
#' @template section_predict_sets
#' @template section_parallelization
#' @template section_progress_bars
#' @template section_logging
#'
#' @template seealso_benchmark
#' @export
#' @examples
#' # benchmarking with benchmark_grid()
#' tasks = lapply(c("penguins", "sonar"), tsk)
#' learners = lapply(c("classif.featureless", "classif.rpart"), lrn)
#' resamplings = rsmp("cv", folds = 3)
#'
#' design = benchmark_grid(tasks, learners, resamplings)
#' print(design)
#'
#' set.seed(123)
#' bmr = benchmark(design)
#'
#' ## Data of all resamplings
#' head(as.data.table(bmr))
#'
#' ## Aggregated performance values
#' aggr = bmr$aggregate()
#' print(aggr)
#'
#' ## Extract predictions of first resampling result
#' rr = aggr$resample_result[[1]]
#' as.data.table(rr$prediction())
#'
#' # Benchmarking with a custom design:
#' # - fit classif.featureless on penguins with a 3-fold CV
#' # - fit classif.rpart on sonar using a holdout
#' tasks = list(tsk("penguins"), tsk("sonar"))
#' learners = list(lrn("classif.featureless"), lrn("classif.rpart"))
#' resamplings = list(rsmp("cv", folds = 3), rsmp("holdout"))
#'
#' design = data.table::data.table(
#'   task = tasks,
#'   learner = learners,
#'   resampling = resamplings
#' )
#'
#' ## Instantiate resamplings
#' design$resampling = Map(
#'   function(task, resampling) resampling$clone()$instantiate(task),
#'   task = design$task, resampling = design$resampling
#' )
#'
#' ## Run benchmark
#' bmr = benchmark(design)
#' print(bmr)
#'
#' ## Get the training set of the 2nd iteration of the featureless learner on penguins
#' rr = bmr$aggregate()[learner_id == "classif.featureless"]$resample_result[[1]]
#' rr$resampling$train_set(2)
benchmark = function(design, store_models = FALSE, store_backends = TRUE, encapsulate = NA_character_, allow_hotstart = FALSE, clone = c("task", "learner", "resampling"), unmarshal = TRUE, callbacks = NULL) {
  assert_subset(clone, c("task", "learner", "resampling"))
  assert_data_frame(design, min.rows = 1L)
  assert_names(names(design), must.include = c("task", "learner", "resampling"))
  assert_flag(unmarshal)
  design$task = list(assert_tasks(as_tasks(design$task)))
  design$learner = list(assert_learners(as_learners(design$learner)))
  design$resampling = list(assert_resamplings(as_resamplings(design$resampling), instantiated = TRUE))
  if (is.null(design$param_values)) {
    design$param_values = list()
  } else {
    design$param_values = list(assert_param_values(design$param_values, n_learners = length(design$learner)))
  }
  assert_flag(store_models)
  assert_flag(store_backends)
  callbacks = assert_callbacks(as_callbacks(callbacks))

  # check for multiple task types
  task_types = unique(map_chr(design$task, "task_type"))
  if (length(task_types) > 1) {
    stopf("Multiple task types detected, but mixing types is not supported: %s", str_collapse(task_types))
  }
  learner_types = unique(map_chr(design$learner, "task_type"))
  if (length(learner_types) > 1) {
    stopf("Multiple learner types detected, but mixing types is not supported: %s", str_collapse(learner_types))
  }

  setDT(design)
  task = learner = resampling = NULL
  if ("task" %chin% clone) {
    design[, "task" := list(list(task[[1L]]$clone())), by = list(hashes(task))]
  }
  if ("learner" %chin% clone) {
    design[, "learner" := list(list(learner[[1L]]$clone())), by = list(hashes(learner))]
  }
  if ("resampling" %chin% clone) {
    design[, "resampling" := list(list(resampling[[1L]]$clone())), by = list(hashes(resampling))]
  }

  # set encapsulation + fallback
  set_encapsulation(design$learner, encapsulate)

  # expand the design: add rows for each resampling iteration and param_values
  grid = pmap_dtr(design, function(task, learner, resampling, param_values) {
    iters = resampling$iters
    n_params = max(1L, length(param_values))
    # insert constant values
    param_values = map(param_values, function(values) insert_named(learner$param_set$values, values))
    assert_learnable(task, learner, unlist(param_values, recursive = FALSE))

    # check that all row ids of the resampling are present in the task
    if (resampling$task_row_hash != task$row_hash) {
      stopf("Resampling '%s' is not instantiated on task '%s'", resampling$id, task$id)
    }

    data.table(
      task = list(task), learner = list(learner), resampling = list(resampling),
      iteration = rep(seq_len(iters), times = n_params),
      param_values = if (is.null(param_values)) list() else rep(param_values, each = iters),
      uhash = rep(UUIDgenerate(n = n_params), each = iters)
    )
  })

  n = nrow(grid)

  # set default mode
  set(grid, j = "mode", value = "train")

  lg$info("Running benchmark with %i resampling iterations", n)
  pb = if (isNamespaceLoaded("progressr")) {
    # NB: the progress bar needs to be created in this env
    pb = progressr::progressor(steps = n)
  } else {
    NULL
  }

  # add hot start learners
  if (allow_hotstart) {
    hotstart_grid = pmap_dtr(grid, function(task, learner, resampling, iteration, ...) {
      if (!is.null(learner$hotstart_stack)) {
        # search for hotstart learner
        learner = learner$clone()
        task_hashes = resampling_task_hashes(task, resampling, learner)
        start_learner = get_private(learner$hotstart_stack)$.start_learner(learner, task_hashes[iteration])
      }
      if (is.null(learner$hotstart_stack) || is.null(start_learner)) {
        # no hotstart learners stored or no adaptable model found
        mode = "train"
      } else {
        # hotstart learner found
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
    set(grid, j = "learner", value = hotstart_grid$learner)
    set(grid, j = "mode", value = hotstart_grid$mode)
  }

  res = future_map(n, workhorse,
    task = grid$task, learner = grid$learner, resampling = grid$resampling, iteration = grid$iteration, param_values = grid$param_values, mode = grid$mode,
    MoreArgs = list(store_models = store_models, lgr_index = lgr::logger_index(), pb = pb, unmarshal = unmarshal, callbacks = callbacks)
  )

  grid = insert_named(grid, list(
    learner_state = map(res, "learner_state"),
    prediction = map(res, "prediction"),
    param_values = map(res, "param_values"),
    learner_hash = map_chr(res, "learner_hash")
  ))

  lg$info("Finished benchmark")

  set(grid, j = "mode", value = NULL)

  data_extra = if (length(callbacks) && any(map_lgl(res, function(x) !is.null(x$data_extra)))) map(res, "data_extra")

  result_data = ResultData$new(grid, data_extra, store_backends = store_backends)

  if (unmarshal && store_models) {
    result_data$unmarshal()
  }

  BenchmarkResult$new(result_data)
}

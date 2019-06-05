#' @title Container for Results of `benchmark()`
#'
#' @usage NULL
#' @format [R6::R6Class] object.
#' @include mlr_reflections.R
#'
#' @description
#' This is the result container object returned by [benchmark()].
#'
#' Note that all stored objects are accessed by reference.
#' Do not modify any object without cloning it first.
#'
#' @section Construction:
#' ```
#' bmr = BenchmarkResult$new(data)
#' ```
#'
#' * `data` :: [data.table::data.table()]\cr
#'   Table with the data of one [Experiment] per row.
#'   See description of field `data` of [Experiment] for the exact structure.
#'
#' @section Fields:
#' * `data` :: [data.table::data.table()]\cr
#'   Experiment data with one [Experiment] per line.
#'
#' * `tasks` :: [data.table::data.table()]\cr
#'   Table of used tasks with three columns:
#'   "task_hash" (`character(1)`), "task_id" (`character(1)`) and "task" ([Task]).
#'
#' * `learners` :: [data.table::data.table()]\cr
#'   Table of used learners with three columns:
#'   "learner_hash" (`character(1)`), "learner_id" (`character(1)`) and "learner" ([Learner]).
#'
#' * `resamplings` :: [data.table::data.table()]\cr
#'   Table of used resamplings with three columns:
#'   "resampling_hash" (`character(1)`), "resampling_id" (`character(1)`) and "resampling" ([Resampling]).
#'
#' * `measures` :: [data.table::data.table()]\cr
#'   Table of used measures with three columns:
#'   "measure_hash" (`character(1)`), "measure_id" (`character(1)`) and "measure" ([Measure]).
#'
#' @section Methods:
#' * `aggregated(objects = TRUE, ids = TRUE, params = FALSE)`\cr
#'   (`logical(1)`, `logical(1)`, `logical(1)`) -> [data.table::data.table()]\cr
#'   Returns a result table where experiments are aggregated per [ResampleResult].
#'   Arguments control the number of additional columns:
#'     * `objects` :: `logical(1)`\cr
#'       Return objects as columns in the result `data.table()`.
#'     * `ids` :: `logical(1)`\cr
#'       Return object ids as columns in the result `data.table()`.
#'     * `params` :: `logical(1)`\cr
#'       Return learner hyperparameter values as list column `params` in the result `data.table()`.
#'
#' * `combine(bmr)`\cr
#'   [BenchmarkResult] -> `self`\cr
#'   Fuses a second [BenchmarkResult] into itself.
#'
#' * `get_best(id)`\cr
#'   (`character(1)`) -> [ResampleResult]\cr
#'   Returns the [ResampleResult] with best performance according to [Measure] with the provided id.
#'
#' @section S3 Methods:
#' * `as.data.table(bmr)`\cr
#'   [BenchmarkResult] -> [data.table::data.table()]\cr
#'   Converts the data to a `data.table()`.
#'
#' @export
#' @examples
#' set.seed(123)
#' design = expand_grid(
#'   tasks = "iris",
#'   learners = c("classif.featureless", "classif.rpart"),
#'   resamplings = "cv3"
#' )
#' print(design)
#'
#' bmr = benchmark(design)
#' print(bmr)
#'
#' bmr$tasks
#' bmr$learners
#' bmr$measures
#'
#' # aggregated results
#' bmr$aggregated()
#'
#' # aggregated results with hyperparameters as separate columns
#' mlr3misc::unnest(bmr$aggregated(params = TRUE), "params")
#'
#' # extract resample result for classif.rpart
#' rr = bmr$aggregated()[learner_id == "classif.rpart", resample_result][[1]]
#' print(rr)
#'
#' # access the confusion matrix of the first resampling iteration
#' rr$experiment(1)$prediction$confusion
BenchmarkResult = R6Class("BenchmarkResult",
  public = list(
    data = NULL,

    initialize = function(data) {
      assert_data_table(data)
      slots = mlr_reflections$experiment_slots$name
      assert_names(names(data), must.include = c(slots, "hash"))
      self$data = setcolorder(data, slots)
    },

    format = function() {
      "<BenchmarkResult>"
    },

    print = function() {
      catf("%s of %i experiments in %i resamplings:",
        format(self), nrow(self$data), uniqueN(self$data$hash))
      measure = self$measures$measure[[1L]]
      aggr = remove_named(self$aggregated(objects = FALSE, ids = TRUE, params = FALSE), "hash")
      setnames(aggr, c("task_id", "learner_id", "resampling_id"), c("task", "learner", "resampling"))
      if (!is.na(measure$minimize)) {
        setorderv(aggr, measure$id, order = -1L + 2L * measure$minimize)
      }
      print(aggr, print.keys = FALSE, class = FALSE, row.names = FALSE)
    },

    combine = function(bmr) {
      assert_benchmark_result(bmr)
      self$data = rbindlist(list(self$data, bmr$data), fill = TRUE, use.names = TRUE)
      invisible(self)
    },

    get_best = function(id) {
      measures = self$measures
      assert_choice(id, measures$measure_id)
      measure = measures[list(id), "measure", on = "measure_id"][[1]][[1L]]
      if (is.na(measure$minimize)) {
        stopf("Impossible to determine best value for measure '%s': '$minimize' is NA", id)
      }
      aggr = self$aggregated(ids = FALSE, objects = FALSE)
      best = if (measure$minimize) which_min else which_max
      aggr[best(get(id)), "resample_result"][[1L]][[1L]]
    },

    aggregated = function(ids = TRUE, objects = TRUE, params = FALSE, unnest_params = FALSE) {

      res = self$data[, list(resample_result = list(ResampleResult$new(.SD))), by = hash]

      if (assert_flag(objects)) {
        extract = function(x) list(task = list(x$task), learner = list(x$learner))
        res = ref_cbind(res, map_dtr(res$resample_result, extract, .fill = TRUE))
      }

      if (assert_flag(ids)) {
        extract = function(x) list(task_id = x$task$id, learner_id = x$learner$id, resampling_id = x$resampling$id)
        res = ref_cbind(res, map_dtr(res$resample_result, extract, .fill = TRUE))
      }

      if (assert_flag(params)) {
        res$params = map(res$resample_result, function(x) x$learner$param_set$values)
      }

      extract = function(x) as.list(x$aggregated)
      res = ref_cbind(res, map_dtr(res$resample_result, extract, .fill = TRUE))

      # if (!objects) {
      #   remove_named(res, "resample_result")
      # }
      res[]
    }
  ),

  active = list(
    tasks = function() {
      unique(self$data[, list(task_hash = hashes(task), task_id = ids(task), task = task)], by = "task_hash")
    },

    learners = function() {
      unique(self$data[, list(learner_hash = hashes(learner), learner_id = ids(learner), learner = learner)], by = "learner_hash")
    },

    resamplings = function() {
      unique(self$data[, list(resampling_hash = hashes(resampling), resampling_id = ids(resampling), resampling = resampling)], by = "resampling_hash")
    },

    measures = function() {
      unique(map_dtr(self$data$measures, function(m) data.table(measure_hash = hashes(m), measure_id = ids(m), measure = m)), by = "measure_id")
    }
  ),

  private = list(
    deep_clone = function(name, value) {
      if (name == "data") copy(value) else value
    }
  )
)

#' @export
as.data.table.BenchmarkResult = function(x, ...) {
  hash = task = learner = resampling = performance = NULL
  unnest(x$data[,
    list(
      hash = hash,
      task = task, task_id = ids(task),
      learner = learner, learner_id = ids(learner),
      resampling = resampling, resampling_id = ids(resampling),
      performance = performance
    )
  ], "performance")
}

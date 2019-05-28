#' @title Container for Results of `benchmark()`
#'
#' @usage NULL
#' @format [R6::R6Class] object.
#' @include mlr_reflections.R
#'
#' @description
#' This is the result container object returned by [benchmark()].
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
#'
#' * `resamplings` :: [data.table::data.table()]\cr
#'   Table of used resamplings with three columns:
#'   "resampling_hash" (`character(1)`), "resampling_id" (`character(1)`) and "resampling" ([Resampling]).
#'
#' * `measures` :: [data.table::data.table()]\cr
#'   Table of used measures with three columns:
#'   "measure_hash" (`character(1)`), "measure_id" (`character(1)`) and "measure" ([Measure]).
#'
#' * `resample_results` :: [data.table::data.table()]\cr
#'   Table of [ResampleResult] groups with 5 columns:
#'   "hash" (`character(1)`), "task_id" (`character(1)`), "learner_id" (`character(1)`), "resampling_id" (`character(1)`) and "N" (`integer(1)`).
#'   The last column ("N") is the number of [Experiment]s in the [ResampleResult].
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
#' * `get_best(measure)`\cr
#'   [Measure] -> [ResampleResult]\cr
#'   Returns the [ResampleResult] with best performance according to the provided [Measure].
#'
#' * `resample_result(hash)`\cr
#'   `character(1)` -> [ResampleResult]\cr
#'   Retrieves the [ResampleResult] whose hash starts with `hash`.
#'   The abbreviation must be unambiguous, so that exactly one [ResampleResult] is matched.
#'
#' @section S3 Methods:
#' * `as.data.table(bmr)`\cr
#'   [BenchmarkResult] -> [data.table::data.table()]\cr
#'   Converts the data to a `data.table()`.
#'
#' @export
#' @examples
#' set.seed(123)
#' bmr = benchmark(expand_grid(
#'   tasks = "iris",
#'   learners = c("classif.featureless", "classif.rpart"),
#'   resamplings = "cv3"
#' ))
#'
#' print(bmr)
#' bmr$tasks
#' bmr$learners
#' bmr$resamplings
#' bmr$measures
#'
#' # aggregated results
#' bmr$aggregated(objects = FALSE)
#'
#' # aggregated results with hyperparameters as separate columns
#' mlr3misc::unnest(bmr$aggregated(objects = FALSE, params = TRUE), "params")
#'
#' # extract resample results and experiments
#' rrs = bmr$resample_results
#' print(rrs)
#' rr = bmr$resample_result(rrs$hash[1])
#' print(rr)
#' rr$experiment(1)$model
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

    resample_result = function(hash) {
      assert_string(hash)
      hashes = self$data[, unique(hash)]
      tmp = hashes[which(startsWith(hash, prefix = hash))]
      if (length(tmp) != 1L) {
        stopf("Hash '%s' matches %i of the recorded resample results", hash, length(tmp))
      }

      ResampleResult$new(self$data[list(tmp), on = "hash", nomatch = 0L], hash = hash)
    },

    combine = function(bmr) {
      assert_benchmark_result(bmr)
      self$data = rbindlist(list(self$data, bmr$data), fill = TRUE, use.names = TRUE)
      invisible(self)
    },

    get_best = function(measure) {
      assert_measure(measure)
      id = measure$id
      if (is.na(measure$minimize)) {
        stopf("Impossible to determine best value for measure '%s': '$minimize' is NA", id)
      }
      aggr = self$aggregated(ids = FALSE)
      if (id %nin% names(aggr)) {
        stopf("Measure with id '%s' not in BenchmarkResult", id)
      }
      best = if (measure$minimize) which_min(aggr[[id]]) else which_max(aggr[[id]])
      aggr$resample_result[[best]]
    },

    aggregated = function(ids = TRUE, objects = TRUE, params = FALSE, unnest_params = FALSE) {

      res = self$data[, list(resample_result = list(ResampleResult$new(.SD))), by = hash]

      if (assert_flag(objects)) {
        extract = function(x) list(task = list(x$task), learner = list(x$learner))
        res = ref_cbind(res, map_dtr(res$resample_result, extract, .fill = TRUE))
      }

      if (assert_flag(ids)) {
        extract = function(x) list(resampling_id = x$resampling$id, task_id = x$task$id, learner_id = x$learner$id)
        res = ref_cbind(res, map_dtr(res$resample_result, extract, .fill = TRUE))
      }

      if (assert_flag(params)) {
        res$params = map(res$resample_result, function(x) x$learner$param_set$values)
      }

      extract = function(x) as.list(x$aggregated)
      res = ref_cbind(res, map_dtr(res$resample_result, extract, .fill = TRUE))

      if (!objects) {
        remove_named(res, "resample_result")
      }
      res[]
    }),

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
    },

    resample_results = function() {
      self$data[, list(task_id = task[[1L]]$id, learner_id = learner[[1L]]$id, resampling_id = resampling[[1L]]$id, .N), by = "hash"]
    }),

  private = list(
    deep_clone = function(name, value) {
      if (name == "data") copy(value) else value
    })
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

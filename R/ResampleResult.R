#' @title Container for Results of `resample()`
#'
#' @usage NULL
#' @format [R6::R6Class] object.
#' @include mlr_reflections.R
#'
#' @description
#' This is the result container object returned by [resample()].
#'
#' Note that all stored objects are accessed by reference.
#' Do not modify any object without cloning it first.
#'
#' @section Construction:
#' ```
#' rr = ResampleResult$new(data)
#' ```
#'
#' * `data` :: [data.table::data.table()]\cr
#'   Table with data for one resampling iteration per row:
#'   [Task], [Learner], [Resampling], iteration (`integer(1)`), and [Prediction].
#'
#' @section Fields:
#' * `data` :: [data.table::data.table()]\cr
#'   Internal data storage.
#'   We discourage users to directly work with this field.
#'
#' * `task` :: [Task]\cr
#'   The task [resample()] operated on.
#'
#' * `learners` :: list of [Learner]\cr
#'   List of trained learners, sorted by resampling iteration.
#'
#' * `resampling` :: [Resampling]\cr
#'   Instantiated [Resampling] object which stores the splits into training and test.
#'
#' * `predictions` :: list of [Prediction]\cr
#'   List of prediction objects, sorted by resampling iteration.
#'
#' * `prediction` :: [Prediction]\cr
#'   Combined [Prediction] of all individual resampling iterations.
#'   Note that the performance of measures is not calculated on this object,
#'   but instead on each iterations separately and then combined with an aggregate function.
#'
#' * `warnings` :: [data.table::data.table()]\cr
#'   Returns a table with all warning messages.
#'   Column names are `"iteration"` and `"msg"`.
#'   Note that there can be multiple rows per resampling iteration if multiple warnings have been recorded.
#'
#' * `errors` :: [data.table::data.table()]\cr
#'   Returns a table with all error messages.
#'   Column names are `"iteration"` and `"msg"`.
#'   Note that there can be multiple rows per resampling iteration if multiple errors have been recorded.
#'
#' * `hash` :: `character(1)`\cr
#'   Hash (unique identifier) for this object.
#'
#' @section Methods:
#' * `combine(rr)`\cr
#'   [ResampleResult] -> [BenchmarkResult]\cr
#'   Takes a second [ResampleResult] and combines both [ResampleResult]s to a [BenchmarkResult].
#'
#' * `performance(measures = NULL, ids = TRUE)`\cr
#'   (`list()` of [Measure], `logical(1)`) -> [data.table::data.table()]\cr
#'   Returns a table with one row for each resampling iteration, including all involved objects.
#'   Additionally calculates the provided performance measures and binds the performance as extra column.
#'   If no measure is provided, defaults to the measure defined in [mlr_reflections$default_measures][mlr_reflections]
#'   ([mlr_measures_classif.ce] for classification and [mlr_measures_regr.mse] for regression).
#'   If `ids` is `TRUE`, character column of id names are added to the table for convenient filtering.
#'
#' * `aggregate(measures = NULL)`\cr
#'   `list()` of [Measure] -> named `numeric()`\cr
#'   Calculates and aggregates performance values for all provided measures.
#'   See [Measure] for the aggregation function.
#'
#' @section S3 Methods:
#' * `as.data.table(rr)`\cr
#'   [ResampleResult] -> [data.table::data.table()]\cr
#'   Returns a copy of the internal data.
#' @export
#' @examples
#' rr = resample("iris", "classif.featureless", "cv3")
#' rr$warnings
#' rr$errors
ResampleResult = R6Class("ResampleResult",
  public = list(
    data = NULL,

    initialize = function(data) {
      assert_data_table(data)
      assert_names(names(data), must.include = mlr_reflections$rr_names)
      self$data = setorderv(data, "iteration")[]
    },

    format = function() {
      sprintf("<%s>", class(self)[1L])
    },

    print = function() {
      catf("%s of %i iterations", format(self), nrow(self$data))
      catf(str_indent("Task:", self$task$id))
      catf(str_indent("Learner:", self$data$learner[[1L]]$id))
    },

    combine = function(rr) {
      assert_resample_result(rr)
      if (self$hash == rr$hash) {
        warningf("ResampleResult$combine(): Identical hashes detected. This is likely to be unintended.")
      }
      BenchmarkResult$new(rbind(cbind(self$data, data.table(hash = self$hash)), cbind(rr$data, data.table(hash = rr$hash))))
    },

    performance = function(measures = NULL, ids = TRUE) {
      measures = assert_measures(measures, task = self$task, learner = self$data$learner[[1L]])
      assert_flag(ids)
      score = function(prediction, task, learner, resampling, iteration) {
        as.list(prediction$score(measures, task = task, learner = learner, train_set = resampling$train_set(iteration)))
      }
      tab = cbind(self$data, pmap_dtr(self$data[, c("prediction", "task", "learner", "resampling", "iteration"), with = FALSE], score))
      if (ids) {
        tab[, c("task_id", "learner_id", "resampling_id") := list(ids(get("task")), ids(get("learner")), ids(get("resampling")))]
        setcolorder(tab, c("task", "task_id", "learner", "learner_id", "resampling", "resampling_id", "iteration", "prediction"))[]
      }
      return(tab)
    },

    aggregate = function(measures = NULL) {
      measures = assert_measures(measures, task = self$task, learner = self$data$learner[[1L]])
      set_names(map_dbl(measures, function(m) m$aggregate(self)), ids(measures))
    }
  ),

  active = list(
    task = function() {
      self$data$task[[1L]]
    },

    learners = function() {
      self$data$learner
    },

    resampling = function() {
      self$data$resampling[[1L]]
    },

    prediction = function() {
      do.call(c, self$data$prediction)
    },

    predictions = function() {
      self$data$prediction
    },

    hash = function() {
      data = self$data
      hash_resample_iteration(data$task[[1L]], data$learner[[1L]], data$resampling[[1L]])
    },

    warnings = function() {
      extract = function(learner) list(warning = learner$warnings)
      rbindlist(map(self$data$learner, extract), idcol = "iteration", use.names = TRUE)
    },

    errors = function() {
      extract = function(learner) list(warning = learner$errors)
      rbindlist(map(self$data$learner, extract), idcol = "iteration", use.names = TRUE)
    }
  ),

  private = list(
    deep_clone = function(name, value) {
      if (name == "data") copy(value) else value
    }
  )
)

#' @export
as.data.table.ResampleResult = function(x, ...) {
  copy(x$data)
}

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
#' * `performance(measures = NULL, ids = TRUE)`\cr
#'   (list of [Measure], `logical(1)`) -> [data.table::data.table()]\cr
#'   Returns a table with one row for each resampling iteration, including all involved objects:
#'   [Task], [Learner], [Resampling], iteration number (`integer(1)`), and [Prediction].
#'   A column with the individual (per resampling iteration) performance is added for each [Measure], named with the id of the respective measure.
#'   If `ids` is `TRUE`, extra columns with the ids of objects (`"task_id"`, `"learner_id"`, `"resampling_id"`) are binded to the table to allow a more convenient subsetting.
#'   If `measures` is `NULL`, `measures` defaults to the return value of [default_measures()].
#'
#' * `aggregate(measures = NULL)`\cr
#'   list of [Measure] -> named `numeric()`\cr
#'   Calculates and aggregates performance values for all provided measures, according to the respective aggregation function in [Measure].
#'   If `measures` is `NULL`, `measures` defaults to the return value of [default_measures()].
#'
#' @section S3 Methods:
#' * `as.data.table(rr)`\cr
#'   [ResampleResult] -> [data.table::data.table()]\cr
#'   Returns a copy of the internal data.
#' @export
#' @examples
#' rr = resample("iris", "classif.featureless", rsmp("cv", folds = 3))
#' print(rr)
#' rr$aggregate("classif.acc")
#' rr$prediction
#' rr$prediction$confusion
#' rr$warnings
#' rr$errors
ResampleResult = R6Class("ResampleResult",
  public = list(
    data = NULL,

    initialize = function(data) {
      assert_data_table(data)
      slots = mlr_reflections$rr_names
      assert_names(names(data), must.include = slots)
      self$data = setcolorder(setcolorder(data, "iteration"), slots)[]
    },

    format = function() {
      sprintf("<%s>", class(self)[1L])
    },

    print = function() {
      catf("%s of %i iterations", format(self), nrow(self$data))
      catf(str_indent("* Task:", self$task$id))
      catf(str_indent("* Learner:", self$data$learner[[1L]]$id))
      perf = self$aggregate()
      catf(str_indent("* Performance:", sprintf("%.3f [%s]", perf, names(perf))))

      warnings = self$warnings
      catf(str_indent("* Warnings:", sprintf("%i in %i iterations", nrow(warnings), uniqueN(warnings, by = "iteration"))))

      errors = self$errors
      catf(str_indent("* Errors:", sprintf("%i in %i iterations", nrow(errors), uniqueN(errors, by = "iteration"))))
    },

    performance = function(measures = NULL, ids = TRUE) {
      measures = as_measures(measures, task_type = self$task$task_type)
      assert_measures(measures, task = self$task, learner = self$learners[[1L]])
      assert_flag(ids)
      tab = copy(self$data)

      if (length(measures)) {
        score = function(prediction, task, learner, resampling, iteration) {
          if (is.null(prediction)) {
            set_names(list(NA_real_), ids(measures))
          } else {
            as.list(prediction$score(measures, task = task, learner = learner, train_set = resampling$train_set(iteration)))
          }
        }
        tab = rcbind(tab, pmap_dtr(self$data[, c("prediction", "task", "learner", "resampling", "iteration"), with = FALSE], score))
      }

      if (ids) {
        tab[, c("task_id", "learner_id", "resampling_id") := list(ids(task), ids(learner), ids(resampling))]
        setcolorder(tab, c("task", "task_id", "learner", "learner_id", "resampling", "resampling_id", "iteration", "prediction"))[]
      }

      tab[]
    },

    aggregate = function(measures = NULL) {
      measures = as_measures(measures, task_type = self$task$task_type)
      assert_measures(measures, task = self$task, learner = self$learners[[1L]])
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
      hash_resample_result(data$task[[1L]], data$learner[[1L]], data$resampling[[1L]])
    },

    warnings = function() {
      extract = function(learner) list(msg = learner$warnings)
      rbindlist(map(self$data$learner, extract), idcol = "iteration", use.names = TRUE)
    },

    errors = function() {
      extract = function(learner) list(msg = learner$errors)
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

#' @rdname as_benchmark_result
#' @export
as_benchmark_result.ResampleResult = function(x, ...) {
  BenchmarkResult$new(cbind(x$data, data.table(hash = x$hash)))
}

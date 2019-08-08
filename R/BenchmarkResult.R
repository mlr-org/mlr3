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
#'   Table with data for one resampling iteration per row:
#'   [Task], [Learner], [Resampling], iteration (`integer(1)`), [Prediction], and the hash (`character(1)`)
#'   of the corresponding [ResampleResult].
#'
#' @section Fields:
#' * `data` :: [data.table::data.table()]\cr
#'   Internal data storage.
#'   We discourage users to directly work with this field.
#'
#' * `tasks` :: [data.table::data.table()]\cr
#'   Table of used tasks with three columns:
#'   `"task_hash"` (`character(1)`), `"task_id"` (`character(1)`) and `"task"` ([Task]).
#'
#' * `learners` :: [data.table::data.table()]\cr
#'   Table of used learners with three columns:
#'   `"learner_hash"` (`character(1)`), `"learner_id"` (`character(1)`) and `"learner"` ([Learner]).
#'
#' * `resamplings` :: [data.table::data.table()]\cr
#'   Table of used resamplings with three columns:
#'   `"resampling_hash"` (`character(1)`), `"resampling_id"` (`character(1)`) and `"resampling"` ([Resampling]).
#'
#' * `hashes` :: `character()`\cr
#'   Vector of hashes of all included [ResampleResult]s.
#'
#' @section Methods:
#' * `aggregate(measures = NULL, ids = TRUE, params = FALSE, warnings = FALSE, errors = FALSE)`\cr
#'   (`list()` of [Measure], `logical(1)`, `logical(1)`, `logical(1)`, `logical(1)`) -> [data.table::data.table()]\cr
#'   Returns a result table where resampling iterations are aggregated together into [ResampleResult]s.
#'   A column with the aggregated performance is added for each [Measure], named with the id of the respective measure.
#'
#'   Additional arguments control the number of additional columns:
#'     * `ids` :: `logical(1)`\cr
#'       Adds object ids (`"task_id"`, `"learner_id"`, `"resampling_id"`) as extra character columns.
#'     * `params` :: `logical(1)`\cr
#'       Adds the hyperparameter values as extra list column `"params"`.
#'       You can unnest them with [mlr3misc::unnest()].
#'     * `warnings` :: `logical(1)`\cr
#'       Adds the number of resampling iterations with at least one warning as extra integer column `"warnings"`.
#'     * `errors` :: `logical(1)`\cr
#'       Adds the number of resampling iterations with errors as extra integer column `"errors"`.
#'   See [mlr_sugar] for the default of `measures`.
#'
#' * `performance(measures = NULL, ids = TRUE)`\cr
#'   (`list()` of [Measure], `logical(1)`) -> [data.table::data.table()]\cr
#'   Returns a table with one row for each resampling iteration, including all involved objects.
#'   Additionally calculates the provided performance measures and binds the performance as extra column.
#'   If `ids` is `TRUE`, character column of id names are added to the table for convenient filtering.
#'   See [mlr_sugar] for the default of `measures`.
#'
#' * `best(measure)`\cr
#'   ([Measure]) -> [ResampleResult]\cr
#'   Returns the [ResampleResult] with the best performance according to [Measure].
#'   See [mlr_sugar] for the default of `measure`.
#'
#' * `resample_result(hash)`\cr
#'   (`character(1)` -> [ResampleResult])\cr
#'   Retrieve the [ResampleResult] with hash `hash`.
#'
#' * `combine(bmr)`\cr
#'   [BenchmarkResult] -> `self`\cr
#'   Fuses a second [BenchmarkResult] into itself, mutating the [BenchmarkResult] in-place.
#'   Note that in case of duplicated [ResampleResult]s, an exception is raised.
#'   Two [ResampleResult]s are identical iff the hashes of the respective [Task], [Learner] and [Resampling] are identical.
#'   I.e., they must operate on the exactly same data, with the same learner with the same hyperparameters and
#'   the same splits into training and test sets.
#'
#' @section S3 Methods:
#' * `as.data.table(bmr)`\cr
#'   [BenchmarkResult] -> [data.table::data.table()]\cr
#'   Returns a copy of the internal data.
#'
#' @export
#' @examples
#' set.seed(123)
#' learners = list(
#'   lrn("classif.featureless", predict_type = "prob"),
#'   lrn("classif.rpart", predict_type = "prob")
#' )
#'
#' design = expand_grid(
#'   tasks = c("sonar", "spam"),
#'   learners = learners,
#'   resamplings = "cv3"
#' )
#' print(design)
#'
#' bmr = benchmark(design)
#' print(bmr)
#'
#' bmr$tasks
#' bmr$learners
#'
#' # first 5 individual resamplings
#' head(as.data.table(bmr, measures = c("classif.acc", "classif.auc")), 5)
#'
#' # aggregate results
#' bmr$aggregate()
#'
#' # aggregate results with hyperparameters as separate columns
#' mlr3misc::unnest(bmr$aggregate(params = TRUE), "params")
#'
#' # extract resample result for classif.rpart
#' rr = bmr$aggregate()[learner_id == "classif.rpart", resample_result][[1]]
#' print(rr)
#'
#' # access the confusion matrix of the first resampling iteration
#' rr$data$prediction[[1]]$confusion
BenchmarkResult = R6Class("BenchmarkResult",
  public = list(
    data = NULL,

    initialize = function(data) {
      assert_data_table(data)
      slots = c("hash", mlr_reflections$rr_names)
      assert_names(names(data), must.include = slots)
      self$data = setcolorder(data, slots)
    },

    format = function() {
      sprintf("<%s>", class(self)[1L])
    },

    print = function() {
      catf("%s of %i rows with %i resampling runs",
        format(self), nrow(self$data), uniqueN(self$data$hash))
      tab = self$aggregate(warnings = TRUE, errors = TRUE)
      tab = remove_named(tab, c("hash", "resample_result"))
      print(tab, class = FALSE, row.names = TRUE, print.keys = FALSE, digits = 3)
    },

    combine = function(bmr) {
      assert_benchmark_result(bmr)
      if (any(self$hashes %in% bmr$hashes)) {
        stop("BenchmarkResult$combine(): Identical hashes detected. Duplicated ResampleResults can not be combined into a single BenchmarkResult.")
      }

      self$data = rbindlist(list(self$data, bmr$data), fill = TRUE, use.names = TRUE)
      invisible(self)
    },

    performance = function(measures = NULL, ids = TRUE) {
      measures = assert_measures(measures, learner = self$data$learner[[1L]])
      assert_flag(ids)
      score = function(prediction, task, learner) as.list(prediction$score(measures, task = task, learner = learner))
      tab = cbind(self$data, pmap_dtr(self$data[, c("prediction", "task", "learner"), with = FALSE], score))

      # replace hash with nr
      tab[, ("nr") := .GRP, by = "hash"][, ("hash") := NULL]

      if (ids) {
        tab[, c("task_id", "learner_id", "resampling_id") := list(ids(task), ids(learner), ids(resampling))]
        setcolorder(tab, c("task", "task_id", "learner", "learner_id", "resampling", "resampling_id", "iteration", "prediction"))
      }

      setcolorder(tab, "nr")[]
    },

    aggregate = function(measures = NULL, ids = TRUE, params = FALSE, warnings = FALSE, errors = FALSE) {
      measures = assert_measures(measures, learner = self$data$learner[[1L]])
      res = self$data[, list(nr = .GRP, resample_result = list(ResampleResult$new(copy(.SD)))), by = hash][, ("hash") := NULL]

      if (assert_flag(ids)) {
        extract = function(x) list(task_id = x$task$id, learner_id = x$learners[[1L]]$id, resampling_id = x$resampling$id)
        res = insert_named(res, map_dtr(res$resample_result, extract))
      }

      if (assert_flag(params)) {
        res[, "params" := list(map(resample_result, function(x) x$learners[[1L]]$param_set$values))]
      }

      if (assert_flag(warnings)) {
        res[, "warnings" := map_int(resample_result, function(rr) uniqueN(rr$warnings, by = "iteration"))]
      }

      if (assert_flag(errors)) {
        res[, "errors" := map_int(resample_result, function(rr) uniqueN(rr$errors, by = "iteration"))]
      }

      rcbind(res, map_dtr(res$resample_result, function(x) as.list(x$aggregate(measures)), .fill = TRUE))
    },

    resample_result = function(i) {
      hashes = self$hashes
      i = assert_int(i, lower = 1L, upper = length(hashes), coerce = TRUE)
      ResampleResult$new(self$data[hash == hashes[i]])
    },

    best = function(measure) {
      measure = assert_measure(measure, learner = self$data$learner[[1L]])
      tab = self$aggregate(measure, ids = FALSE)
      best = if (measure$minimize) which_min else which_max
      tab$resample_result[[best(tab[[measure$id]])]]
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

    hashes = function() {
      hash = NULL
      self$data[, unique(hash)]
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
  tab = copy(x$data)
}

#' @title Convert to BenchmarkResult
#'
#' @description
#' Simple S3 method to convert objects to a [BenchmarkResult].
#'
#' @param x :: `any`\cr
#'  Object to dispatch on, e.g. a [ResampleResult].
#' @param ... :: `any`\cr
#'  Currently not used.
#'
#' @return ([BenchmarkResult]).
#' @export
as_benchmark_result = function(x, ...) {
  UseMethod("as_benchmark_result")
}

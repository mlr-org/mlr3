#' @title Container for Results of `benchmark()`
#'
#' @usage NULL
#' @format [R6::R6Class] object.
#' @include mlr_reflections.R
#'
#' @description
#' This is the result container object returned by [benchmark()].
#' A [BenchmarkResult] consists of the data row-binded data of multiple [ResampleResult]s,
#' which can easily be re-constructed.
#'
#' Note that all stored objects are accessed by reference.
#' Do not modify any object without cloning it first.
#'
#' @section Construction:
#' ```
#' bmr = BenchmarkResult$new(data = data.table())
#' ```
#'
#' * `data` :: [data.table::data.table()]\cr
#'   Table with data for one resampling iteration per row:
#'   [Task], [Learner], [Resampling], iteration (`integer(1)`), [Prediction], and the hash (`character(1)`)
#'   of the corresponding [ResampleResult].
#'   Additional columns are kept in the resulting object.
#'
#' @section Fields:
#' * `data` :: [data.table::data.table()]\cr
#'   Internal data storage with one row per resampling iteration.
#'   Can be joined with `$rr_data` by joining on column `"hash"`.
#'   We discourage users to directly work with this table.
#'
#'   Package develops on the other hand may opt to add additional columns here.
#'   These columns are preserved in all mutators.
#'
#' * `rr_data` :: [data.table::data.table()]\cr
#'   Internal data storage with one row per [ResampleResult].
#'   Can be joined with `$data` by joining on column `"hash"`.
#'   Not used in `mlr3` directly, but can be exploited by add-on packages.
#'
#'   Package develops may opt to add additional columns here.
#'   These columns are preserved in all mutators.
#'
#' * `task_type` :: `character(1)`\cr
#'   Task type of objects in the `BenchmarkResult`.
#'   All stored objects ([Task], [Learner], [Prediction]) in a single `BenchmarkResult` are required to have the same task type, e.g., `"classif"` or `"regr"`.
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
#' * `n_resample_results` :: `integer(1)`\cr
#'   Returns the number of stored [ResampleResult]s.
#'
#' * `hashes` :: `character()`\cr
#'   Vector of hashes of all included [ResampleResult]s.
#'
#' @section Methods:
#' * `aggregate(measures = NULL, ids = TRUE, hashes = FALSE, params = FALSE, conditions = FALSE)`\cr
#'   (list of [Measure], `logical(1)`, `logical(1)`, `logical(1)`, `logical(1)`) -> [data.table::data.table()]\cr
#'   Returns a result table where resampling iterations are combined into [ResampleResult]s.
#'   A column with the aggregated performance score is added for each [Measure], named with the id of the respective measure.
#'
#'   For convenience, the following parameters can be set to extract more information from the returned [ResampleResult]:
#'     * `hashes` :: `logical(1)`\cr
#'       Adds the hash values of the [ResampleResult] as extra character column `"hash"`.
#'     * `ids` :: `logical(1)`\cr
#'       Adds object ids (`"task_id"`, `"learner_id"`, `"resampling_id"`) as extra character columns.
#'     * `params` :: `logical(1)`\cr
#'       Adds the hyperparameter values as extra list column `"params"`.
#'       You can unnest them with [mlr3misc::unnest()].
#'     * `conditions` :: `logical(1)`\cr
#'       Adds the number of resampling iterations with at least one warning as extra integer column `"warnings"`, and
#'       the number of resampling iterations with errors as extra integer column `"errors"`.
#'
#' * `score(measures = NULL, ids = TRUE)`\cr
#'   (list of [Measure], `logical(1)`) -> [data.table::data.table()]\cr
#'   Returns a table with one row for each resampling iteration, including all involved objects:
#'   [Task], [Learner], [Resampling], iteration number (`integer(1)`), and [Prediction].
#'   If `ids` is set to `TRUE`, character column of extracted ids are added to the table for convenient filtering: `"task_id"`, `"learner_id"`, and `"resampling_id"`.
#'   Additionally calculates the provided performance measures and binds the performance as extra columns.
#'   These columns are named using the id of the respective [Measure].
#'
#' * `resample_result(i = NULL, hash = NULL)`\cr
#'   (`integer(1)`, `character(1)`) -> [ResampleResult]\cr
#'   Retrieve the i-th [ResampleResult], by position or by hash.
#'   `i` and `hash` are mutually exclusive.
#'
#' * `combine(bmr)`\cr
#'   ([BenchmarkResult] | `NULL`) -> `self`\cr
#'   Fuses a second [BenchmarkResult] into itself, mutating the [BenchmarkResult] in-place.
#'   If `bmr` is `NULL`, simply returns `self`.
#'
#'   In case of duplicated [ResampleResult]s, an exception is raised.
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
#' design = benchmark_grid(
#'   tasks = list(tsk("sonar"), tsk("spam")),
#'   learners = learners,
#'   resamplings = rsmp("cv", folds = 3)
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
#' rr$predictions()[[1]]$confusion
BenchmarkResult = R6Class("BenchmarkResult",
  public = list(
    data = NULL,
    rr_data = NULL,

    initialize = function(data = data.table()) {
      assert_data_table(data)
      slots = c("hash", mlr_reflections$rr_names)
      if (any(dim(data) == 0L)) {
        data = data.table(hash = character(), task = list(), learner = list(), resampling = list(),
          iteration = integer(), prediction = list())
      } else {
        assert_names(names(data), must.include = slots)
      }

      self$data = setcolorder(data, slots)
      self$rr_data = data[, list(hash = unique(hash))]
    },

    format = function() {
      sprintf("<%s>", class(self)[1L])
    },

    print = function() {
      tab = remove_named(self$aggregate(measures = list(), conditions = TRUE), c("hash", "resample_result"))
      catf("%s of %i rows with %i resampling runs",
        format(self), nrow(self$data), nrow(tab))
      if (nrow(tab)) {
        print(tab, class = FALSE, row.names = FALSE, print.keys = FALSE, digits = 3)
      }
    },

    combine = function(bmr) {
      if (!is.null(bmr)) {
        assert_benchmark_result(bmr)
        if (any(self$hashes %in% bmr$hashes)) {
          stop("BenchmarkResult$combine(): Identical hashes detected. Duplicated ResampleResults can not be combined into a single BenchmarkResult.")
        }
        self$data = rbindlist(list(self$data, bmr$data), fill = TRUE, use.names = TRUE)
        self$rr_data = rbindlist(list(self$rr_data, bmr$rr_data), fill = TRUE, use.names = TRUE)
      }

      invisible(self)
    },

    score = function(measures = NULL, ids = TRUE) {
      measures = as_measures(measures, task_type = self$task_type)
      assert_measures(measures)
      assert_flag(ids)
      tab = copy(self$data)

      for (m in measures) {
        set(tab, j = m$id, value = measure_score_data(m, self$data))
      }

      # replace hash with nr
      tab[, ("nr") := .GRP, by = "hash"][, ("hash") := NULL]

      if (ids) {
        tab[, c("task_id", "learner_id", "resampling_id") := list(ids(task), ids(learner), ids(resampling))]
        setcolorder(tab, c("nr", "task", "task_id", "learner", "learner_id", "resampling", "resampling_id", "iteration", "prediction"))
      } else {
        setcolorder(tab, "nr")
      }

      tab[]
    },

    aggregate = function(measures = NULL, ids = TRUE, hashes = FALSE, params = FALSE, conditions = FALSE) {
      res = self$data[, list(nr = .GRP, iters = .N, resample_result = list(ResampleResult$new(copy(.SD)))), by = hash]

      if (assert_flag(ids)) {
        res[, "task_id" := map_chr(resample_result, function(x) x$task$id)]
        res[, "learner_id" := map_chr(resample_result, function(x) x$learners[[1L]]$id)]
        res[, "resampling_id" := map_chr(resample_result, function(x) x$resampling$id)]
      }

      # move iters to last column
      setcolorder(res, names(res)[-3L])

      if (assert_flag(params)) {
        res[, "params" := list(map(resample_result, function(x) x$learners[[1L]]$param_set$values))]
      }

      if (assert_flag(conditions)) {
        res[, "warnings" := map_int(resample_result, function(rr) uniqueN(rr$warnings, by = "iteration"))]
        res[, "errors" := map_int(resample_result, function(rr) uniqueN(rr$errors, by = "iteration"))]
      }

      if (nrow(res)) {
        res = rcbind(res, map_dtr(res$resample_result, function(x) as.list(x$aggregate(measures)), .fill = TRUE))
      }

      if (ncol(self$rr_data) >= 2L) {
        res = merge(res, self$rr_data, on = "hash", all.x = TRUE, all.y = FALSE, sort = FALSE)
      }

      if (!assert_flag(hashes)) {
        res[, ("hash") := NULL]
      } else {
        setcolorder(res, c("nr", "hash"))
      }

      return(res[])
    },

    resample_result = function(i = NULL, hash = NULL) {
      if (!xor(is.null(i), is.null(hash))) {
        stopf("Either `i` or `hash` must be provided")
      }

      hashes = self$hashes
      if (is.null(i)) {
        needle = assert_choice(hash, hashes)
      } else {
        i = assert_int(i, lower = 1L, upper = length(hashes), coerce = TRUE)
        needle = hashes[i]
      }
      ResampleResult$new(self$data[hash == needle])
    }
  ),

  active = list(
    task_type = function() {
      if (nrow(self$data) == 0L)
        return(NULL)
      self$data$task[[1L]]$task_type
    },

    tasks = function() {
      unique(self$data[, list(task_hash = hashes(task), task_id = ids(task), task = task)], by = "task_hash")
    },

    learners = function() {
      unique(self$data[, list(learner_hash = hashes(learner), learner_id = ids(learner), learner = learner)], by = "learner_hash")
    },

    resamplings = function() {
      unique(self$data[, list(resampling_hash = hashes(resampling), resampling_id = ids(resampling), resampling = resampling)], by = "resampling_hash")
    },

    n_resample_results = function() {
      nrow(self$rr_data)
    },

    hashes = function() {
      self$rr_data$hash
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
  copy(x$data)
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

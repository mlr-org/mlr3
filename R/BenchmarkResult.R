#' @title Container for Results of `benchmark()`
#'
#' @include mlr_reflections.R
#'
#' @description
#' This is the result container object returned by [benchmark()].
#' A [BenchmarkResult] consists of the data row-binded data of multiple
#' [ResampleResult]s, which can easily be re-constructed.
#'
#' Note that all stored objects are accessed by reference.
#' Do not modify any object without cloning it first.
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

    #' @field data [data.table::data.table()]\cr
    #'   Internal data storage with one row per resampling iteration.
    #'   Can be joined with `$rr_data` by joining on column `"hash"`.
    #'   We discourage users to directly work with this table.
    data = NULL,


    #' @field rr_data [data.table::data.table()]\cr
    #'   Internal data storage with one row per [ResampleResult].
    #'   Can be joined with `$data` by joining on column `"hash"`.
    #'   Not used in `mlr3` directly, but can be exploited by add-on packages.
    #'
    #'   Package develops may opt to add additional columns here.
    #'   These columns are preserved in all mutators.
    rr_data = NULL,

    #' @description
    #' Creates a new instance of the [R6][R6::R6Class] object.
    #'
    #' @param data [data.table::data.table()]\cr
    #'   Table with data for one resampling iteration per row: [Task],
    #'   [Learner], [Resampling], iteration (`integer(1)`), [Prediction], and
    #'   the unique hash `uhash` (`character(1)`) of the corresponding
    #'   [ResampleResult]. Additional columns are kept in the resulting object.
    initialize = function(data = data.table()) {
      assert_data_table(data)
      slots = c("uhash", mlr_reflections$rr_names)
      if (any(dim(data) == 0L)) {
        data = data.table(uhash = character(), task = list(), learner = list(), resampling = list(),
          iteration = integer(), prediction = list())
      } else {
        assert_names(names(data), must.include = slots)
      }

      self$data = setcolorder(data, slots)
      self$rr_data = data[, list(uhash = unique(uhash))]
    },

    #' @description
    #'   Opens the help page for this object.
    help = function() {
      open_help("mlr3::BenchmarkResult")
    },

    #' @description
    #' Helper for print outputs.
    format = function() {
      sprintf("<%s>", class(self)[1L])
    },

    #' @description
    #' Printer.
    print = function() {
      tab = remove_named(self$aggregate(measures = list(), conditions = TRUE), c("uhash", "resample_result"))
      catf("%s of %i rows with %i resampling runs",
        format(self), nrow(self$data), nrow(tab))
      if (nrow(tab)) {
        print(tab, class = FALSE, row.names = FALSE, print.keys = FALSE, digits = 3)
      }
    },

    #' @description
    #'  Fuses a second [BenchmarkResult] into itself, mutating the
    #'  [BenchmarkResult] in-place. If `bmr` is `NULL`, simply returns `self`.
    #' @param bmr [BenchmarkResult]\cr
    #'  A [BenchmarkResult] object.
    combine = function(bmr) {
      if (!is.null(bmr)) {
        assert_benchmark_result(bmr)
        if (nrow(self$data) && self$task_type != bmr$task_type) {
          stopf("BenchmarkResult is of task type '%s', but must be '%s'", bmr$task_type, self$task_type)
        }
        self$data = rbindlist(list(self$data, bmr$data), fill = TRUE, use.names = TRUE)
        self$rr_data = rbindlist(list(self$rr_data, bmr$rr_data), fill = TRUE, use.names = TRUE)
      }

      invisible(self)
    },


    #' @description
    #'   Returns a table with one row for each resampling iteration, including
    #'   all involved objects: [Task], [Learner], [Resampling], iteration number
    #'   (`integer(1)`), and [Prediction]. If `ids` is set to `TRUE`, character
    #'   column of extracted ids are added to the table for convenient
    #'   filtering: `"task_id"`, `"learner_id"`, and `"resampling_id"`.
    #'   Additionally calculates the provided performance measures and binds the
    #'   performance as extra columns. These columns are named using the id of
    #'   the respective [Measure].
    #' @param measures [Measure]\cr
    #'   [Measure](s) to calculate the score for.
    #' @param ids `logical(1)`\cr
    #'   Adds object ids (`"task_id"`, `"learner_id"`, `"resampling_id"`) as
    #'   extra character columns.
    score = function(measures = NULL, ids = TRUE) {

      measures = assert_measures(as_measures(measures, task_type = self$task_type))
      assert_flag(ids)
      tab = copy(self$data)

      for (m in measures) {
        set(tab, j = m$id, value = measure_score_data(m, self$data))
      }

      # replace hash with nr
      tab[, ("nr") := .GRP, by = "uhash"][, ("uhash") := NULL]

      if (ids) {
        tab[, c("task_id", "learner_id", "resampling_id") := list(ids(task), ids(learner), ids(resampling))]
        setcolorder(tab, c("nr", "task", "task_id", "learner", "learner_id", "resampling", "resampling_id", "iteration", "prediction"))
      } else {
        setcolorder(tab, "nr")
      }

      tab[]
    },

    #' @description
    #' Returns a result table where resampling iterations are combined into
    #' [ResampleResult]s. A column with the aggregated performance score is
    #' added for each [Measure], named with the id of the respective measure.
    #'
    #' For convenience, different flags can be set to extract more
    #' information from the returned [ResampleResult]:
    #'
    #' @param measures ([Measure] | list of [Measure])\cr
    #'   List of performance measures to calculate.
    #'
    #' @param uhashes `logical(1)`\cr
    #'   Adds the uhash values of the [ResampleResult] as extra character
    #'   column `"uhash"`.
    #'
    #' @param ids `logical(1)`\cr
    #'   Adds object ids (`"task_id"`, `"learner_id"`, `"resampling_id"`) as
    #'   extra character columns.
    #'
    #' @param params `logical(1)`\cr
    #'   Adds the hyperparameter values as extra list column `"params"`. You
    #'   can unnest them with [mlr3misc::unnest()].
    #'
    #' @param conditions `logical(1)`\cr
    #'   Adds the number of resampling iterations with at least one warning as
    #'   extra integer column `"warnings"`, and the number of resampling
    #'   iterations with errors as extra integer column `"errors"`.
    aggregate = function(measures = NULL, ids = TRUE, uhashes = FALSE,
      params = FALSE, conditions = FALSE) {

      res = self$data[, list(
        nr = .GRP,
        iters = .N,
        resample_result = list(if (.N > 0L) ResampleResult$new(copy(.SD), uhash[1L]) else NULL)
      ), by = uhash]

      if (assert_flag(ids)) {
        res[, "task_id" := map_chr(resample_result, function(x) x$task$id)]
        res[, "learner_id" := map_chr(resample_result, function(x) x$learners[[1L]]$id)]
        res[, "resampling_id" := map_chr(resample_result, function(x) x$resampling$id)]
      }

      # move iters to last column
      setcolorder(res, setdiff(names(res), "iters"))

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
        res = merge(res, self$rr_data, on = "uhash", all.x = TRUE, all.y = FALSE, sort = FALSE)
      }

      if (!assert_flag(uhashes)) {
        res[, ("uhash") := NULL]
      } else {
        setcolorder(res, c("nr", "uhash"))
      }

      return(res[])
    },

    #' @description
    #'   Subsets the benchmark result. If `task_ids` is not `NULL`, keeps all
    #'   tasks with provided task ids while discards all others. Same procedure
    #'   for `learner_ids` and `resampling_ids`.
    #' @param task_ids (`character()`)\cr
    #'   Ids of [Task]s to keep.
    #' @param learner_ids (`character()`)\cr
    #'   Ids of [Learner]s to keep.
    #' @param resampling_ids (`character()`)\cr
    #'   Ids of [Resampling]s to keep.
    filter = function(task_ids = NULL, learner_ids = NULL, resampling_ids = NULL) {
      if (!is.null(task_ids)) {
        assert_character(task_ids, any.missing = FALSE)
        self$data = self$data[ids(task) %in% task_ids]
      }

      if (!is.null(learner_ids)) {
        assert_character(learner_ids, any.missing = FALSE)
        self$data = self$data[ids(learner) %in% learner_ids]
      }

      if (!is.null(resampling_ids)) {
        assert_character(resampling_ids, any.missing = FALSE)
        self$data = self$data[ids(resampling) %in% resampling_ids]
      }

      invisible(self)
    },

    #' @description
    #'   Retrieve the i-th [ResampleResult], by position or by unique hash
    #'   `uhash`. `i` and `uhash` are mutually exclusive.
    #' @param i `integer()`\cr
    #'   The desired iteration value.
    #' @param uhash `logical(1)`\cr
    #'   The desired `ushash` value.
    resample_result = function(i = NULL, uhash = NULL) {
      if (!xor(is.null(i), is.null(uhash))) {
        stopf("Either `i` or `uhash` must be provided")
      }

      uhashes = self$uhashes
      if (is.null(i)) {
        needle = assert_choice(uhash, uhashes)
      } else {
        i = assert_int(i, lower = 1L, upper = length(uhashes), coerce = TRUE)
        needle = uhashes[i]
      }
      ResampleResult$new(self$data[uhash == needle])
    }
  ),

  active = list(

    #' @field task_type `character(1)`\cr
    #'   Task type of objects in the `BenchmarkResult`. All stored objects
    #'   ([Task], [Learner], [Prediction]) in a single `BenchmarkResult` are
    #'   required to have the same task type, e.g., `"classif"` or `"regr"`.
    #'
    task_type = function(rhs) {
      assert_ro_binding(rhs)
      if (nrow(self$data) == 0L) {
        return(NULL)
      }
      self$data$task[[1L]]$task_type
    },

    #' @field tasks [data.table::data.table()]\cr
    #'   Table of used tasks with three columns:
    #'   `"task_hash"` (`character(1)`), `"task_id"` (`character(1)`) and
    #'   `"task"` ([Task]).
    #
    tasks = function(rhs) {
      assert_ro_binding(rhs)
      unique(self$data[, list(task_hash = hashes(task), task_id = ids(task), task = task)], by = "task_hash")
    },

    #' @field learners [data.table::data.table()]\cr
    #'   Table of used learners with three columns:
    #'   `"learner_hash"` (`character(1)`), `"learner_id"` (`character(1)`) and
    #'   `"learner"` ([Learner]).
    #'
    #'   Note that it is not feasible to access learned models via this getter,
    #'   as the training task would be ambiguous. Instead, select a row from the
    #'   table returned by `$score()`.
    #
    learners = function(rhs) {
      assert_ro_binding(rhs)
      tab = unique(self$data[, list(learner_hash = hashes(learner), learner_id = ids(learner), learner = learner)], by = "learner_hash")
      tab[, learner := lapply(learner, function(x) x$clone(deep = TRUE)$reset())][]
    },

    #' @field resamplings [data.table::data.table()]\cr
    #'   Table of used resamplings with three columns: `"resampling_hash"`
    #'   (`character(1)`), `"resampling_id"` (`character(1)`) and `"resampling"`
    #'   ([Resampling]).
    resamplings = function(rhs) {
      assert_ro_binding(rhs)
      unique(self$data[, list(resampling_hash = hashes(resampling), resampling_id = ids(resampling), resampling = resampling)], by = "resampling_hash")
    },

    #' @field n_resample_results `integer(1)`\cr
    #'   Returns the number of stored [ResampleResult]s.
    n_resample_results = function(rhs) {
      assert_ro_binding(rhs)
      nrow(self$rr_data)
    },

    #' @field uhashes `character()`\cr
    #'   Vector of unique hashes of all included [ResampleResult]s.
    uhashes = function(rhs) {
      assert_ro_binding(rhs)
      self$rr_data$uhash
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

#' @importFrom stats friedman.test
#' @export
friedman.test.BenchmarkResult = function(y, measure = NULL, ...) {
  # FIXME: this must be documented somewhere
  measure = assert_measure(as_measure(measure, task_type = y$task_type))
  aggr = y$aggregate(measure)
  friedman.test(aggr[[measure$id]], aggr$learner_id, aggr$task_id)
}

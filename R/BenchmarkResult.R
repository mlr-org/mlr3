#' @title Container for Benchmarking Results
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
#' @template param_measures
#'
#' @section S3 Methods:
#' * `as.data.table(rr, ..., reassemble_learners = TRUE, convert_predictions = TRUE, predict_sets = "test")`\cr
#'   [BenchmarkResult] -> [data.table::data.table()]\cr
#'   Returns a tabular view of the internal data.
#' * `c(...)`\cr
#'   ([BenchmarkResult], ...) -> [BenchmarkResult]\cr
#'   Combines multiple objects convertible to [BenchmarkResult] into a new [BenchmarkResult].
#' * `friedman.test(y, ...)`\cr
#'   [BenchmarkResult] -> `"htest"`\cr
#'   Applies [friedman.test()] on the benchmark result, returning an
#'   object of class `"htest"`.
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
    #' @field data ([data.table::data.table()])\cr
    #' Internal tabular representation of the data.
    #' Tasks, learners and resamplings are references by their hash.
    #' The referenced objects can be accessed via `$tasks`, `$learners` or `$resamplings`, respectively.
    data = NULL,

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param data ([data.table::data.table()])\cr
    #'   Table with data for one resampling iteration per row, with at least the following columns:
    #'
    #'   * `"task"` ([Task]),
    #'   * `"learner"` ([Learner]),
    #'   * `"state"` (`list()`),
    #'   * `"resampling"` ([Resampling]),
    #'   * `"iteration"` (`integer()`),
    #'   * `"prediction"` ([PredictionData]), and
    #'   * `"uhash"` (`character()`).
    #'
    #'   Column `"uhash"` is the unique hash of the corresponding [ResampleResult].
    #'   Additional columns are kept in the resulting object, but otherwise ignored by [BenchmarkResult].
    initialize = function(data = data.table()) {
      assert_data_table(data)
      slots = c("uhash", mlr_reflections$rr_names, "state")
      if (any(dim(data) == 0L)) {
        data = data.table(uhash = character(), task = list(), learner = list(), state = list(),
          resampling = list(), iteration = integer(), prediction = list())
      } else {
        assert_names(names(data), must.include = slots)
      }

      self$data = as_snowflake(data)
    },

    #' @description
    #' Opens the help page for this object.
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
        format(self), nrow(self$data$fact), nrow(tab))
      if (nrow(tab)) {
        print(tab, class = FALSE, row.names = FALSE, print.keys = FALSE, digits = 3)
      }
    },

    #' @description
    #' Fuses a second [BenchmarkResult] into itself, mutating the [BenchmarkResult] in-place.
    #' If the second [BenchmarkResult] `bmr` is `NULL`, simply returns `self`.
    #' Note that you can alternatively use the combine function [c()] which calls this method internally.
    #'
    #' @param bmr ([BenchmarkResult])\cr
    #'   A second [BenchmarkResult] object.
    #'
    #' @return
    #' Returns the object itself, but modified **by reference**.
    #' You need to explicitly `$clone()` the object beforehand if you want to keeps
    #' the object in its previous state.
    combine = function(bmr) {
      if (!is.null(bmr)) {
        assert_benchmark_result(bmr)
        if (nrow(self$data$fact) && self$task_type != bmr$task_type) {
          stopf("BenchmarkResult is of task type '%s', but must be '%s'", bmr$task_type, self$task_type)
        }

        self$data = snowflake_append(self$data, bmr$data)
      }

      invisible(self)
    },


    #' @description
    #' Returns a table with one row for each resampling iteration, including
    #' all involved objects: [Task], [Learner], [Resampling], iteration number
    #' (`integer(1)`), and [Prediction]. If `ids` is set to `TRUE`, character
    #' column of extracted ids are added to the table for convenient
    #' filtering: `"task_id"`, `"learner_id"`, and `"resampling_id"`.
    #'
    #' Additionally calculates the provided performance measures and binds the
    #' performance scores as extra columns. These columns are named using the id of
    #' the respective [Measure].
    #'
    #' @param ids (`logical(1)`)\cr
    #'   Adds object ids (`"task_id"`, `"learner_id"`, `"resampling_id"`) as
    #'   extra character columns for convenient subsetting.
    #'
    #' @param predict_sets (`character()`)\cr
    #'   Vector of predict sets (`{"train", "test"}`) to construct the [Prediction] objects from.
    #'   Default is `"test"`.
    #'
    #' @return [data.table::data.table()].
    score = function(measures = NULL, ids = TRUE, predict_sets = "test") {
      measures = assert_measures(as_measures(measures, task_type = self$task_type))
      assert_flag(ids)

      tab = score_measures(self, measures)

      tab[, ("nr") := .GRP, by = "uhash"][, ("uhash") := NULL]
      if (ids) {
        set(tab, j = "task_id", value = ids(tab$task))
        set(tab, j = "learner_id", value = ids(tab$learner))
        set(tab, j = "resampling_id", value = ids(tab$resampling))
        setcolorder(tab, c("nr", "task", "task_id", "learner", "learner_id",
            "resampling", "resampling_id", "iteration", "prediction"))
      } else {
        setcolorder(tab, "nr")
      }

      # FIXME: this already got converted
      # set(tab, j = "prediction", value = as_predictions(tab$prediction, predict_sets))
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
    #' @param uhashes (`logical(1)`)\cr
    #'   Adds the uhash values of the [ResampleResult] as extra character
    #'   column `"uhash"`.
    #'
    #' @param ids (`logical(1)`)\cr
    #'   Adds object ids (`"task_id"`, `"learner_id"`, `"resampling_id"`) as
    #'   extra character columns for convenient subsetting.
    #'
    #' @param params (`logical(1)`)\cr
    #'   Adds the hyperparameter values as extra list column `"params"`. You
    #'   can unnest them with [mlr3misc::unnest()].
    #'
    #' @param conditions (`logical(1)`)\cr
    #'   Adds the number of resampling iterations with at least one warning as
    #'   extra integer column `"warnings"`, and the number of resampling
    #'   iterations with errors as extra integer column `"errors"`.
    #'
    #' @return [data.table::data.table()].
    aggregate = function(measures = NULL, ids = TRUE, uhashes = FALSE, params = FALSE, conditions = FALSE) {
      tab = self$resample_results
      rrs = tab$resample_result
      set(tab, j = "nr", value = seq_row(tab))

      if (assert_flag(ids)) {
        set(tab, j = "task_id",
          value = map_chr(rrs, function(rr) rr$task$id))
        set(tab, j = "learner_id",
          value = map_chr(rrs, function(rr) rr$learner$id))
        set(tab, j = "resampling_id",
          value = map_chr(rrs, function(rr) rr$resampling$id))
      }

      # move iters to last column
      setcolorder(tab, setdiff(names(tab), "iters"))

      if (assert_flag(params)) {
        set(tab, j = "params",
          value = list(map(rrs, function(x) x$learner$param_set$values)))
      }

      if (assert_flag(conditions)) {
        set(tab, j = "warnings",
          value = map_int(rrs, function(rr) uniqueN(rr$warnings, by = "iteration")))
        set(tab, j = "errors",
          value = map_int(rrs, function(rr) uniqueN(rr$errors, by = "iteration")))
      }

      if (nrow(tab)) {
        tab = rcbind(tab, map_dtr(rrs, function(x) as.list(x$aggregate(measures)), .fill = TRUE))
      }

      if (!assert_flag(uhashes)) {
        set(tab, j = "uhash", value = NULL)
        setcolorder(tab, "nr")
      } else {
        setcolorder(tab, c("nr", "uhash"))
      }

      return(tab[])
    },

    #' @description
    #' Subsets the benchmark result. If `task_ids` is not `NULL`, keeps all
    #' tasks with provided task ids and discards all others tasks.
    #' Same procedure for `learner_ids` and `resampling_ids`.
    #'
    #' @param task_ids (`character()`)\cr
    #'   Ids of [Task]s to keep.
    #' @param task_hashes (`character()`)\cr
    #'   Hashes of [Task]s to keep.
    #' @param learner_ids (`character()`)\cr
    #'   Ids of [Learner]s to keep.
    #' @param learner_hashes (`character()`)\cr
    #'   Hashes of [Learner]s to keep.
    #' @param resampling_ids (`character()`)\cr
    #'   Ids of [Resampling]s to keep.
    #' @param resampling_hashes (`character()`)\cr
    #'   Hashes of [Resampling]s to keep.
    #'
    #' @return
    #' Returns the object itself, but modified **by reference**.
    #' You need to explicitly `$clone()` the object beforehand if you want to keeps
    #' the object in its previous state.
    filter = function(task_ids = NULL, task_hashes = NULL, learner_ids = NULL, learner_hashes = NULL,
      resampling_ids = NULL, resampling_hashes = NULL) {

      filter_by_col = function(tab, column, values) {
        # %in% preserves the key
        self$data[[tab]] = self$data[[tab]][get(column) %in% values]
      }

      if (!is.null(task_ids)) {
        assert_character(task_ids, any.missing = FALSE)
        filter_by_col("tasks", "task_id", task_ids)
      }

      if (!is.null(task_hashes)) {
        assert_character(task_ids, any.missing = FALSE)
        filter_by_col("tasks", "task_hash", task_hashes)
      }

      if (!is.null(learner_ids)) {
        assert_character(learner_ids, any.missing = FALSE)
        filter_by_col("learners", "learner_id", learner_ids)
      }

      if (!is.null(learner_hashes)) {
        assert_character(learner_hashes, any.missing = FALSE)
        filter_by_col("learners", "learner_hash", learner_hashes)
      }

      if (!is.null(resampling_ids)) {
        assert_character(resampling_ids, any.missing = FALSE)
        filter_by_col("resamplings", "resampling_id", resampling_ids)
      }

      if (!is.null(resampling_hashes)) {
        assert_character(resampling_hashes, any.missing = FALSE)
        filter_by_col("resamplings", "resampling_hash", resamplings_hashes)
      }

      filter_by_col("task_objs", "task_phash", self$data$tasks$task_phash)
      filter_by_col("learner_objs", "learner_phash", self$data$learners$learner_phash)
      filter_by_col("uhash", "task_hash", self$data$tasks$task_hash)
      filter_by_col("uhash", "learner_hash", self$data$learners$learner_hash)
      filter_by_col("fact", "uhash", self$data$uhash$uhash)

      invisible(self)
    },

    #' @description
    #' Retrieve the i-th [ResampleResult], by position or by unique hash `uhash`.
    #' `i` and `uhash` are mutually exclusive.
    #'
    #' @param i (`integer(1)`)\cr
    #'   The iteration value to filter for.
    #'
    #' @param uhash (`logical(1)`)\cr
    #'   The `ushash` value to filter for.
    #'
    #' @return [ResampleResult].
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

      snowflake_rr_result(self$data, needle)
    }
  ),

  active = list(
    #' @field task_type (`character(1)`)\cr
    #' Task type of objects in the `BenchmarkResult`.
    #' All stored objects ([Task], [Learner], [Prediction]) in a single `BenchmarkResult` are
    #' required to have the same task type, e.g., `"classif"` or `"regr"`.
    #' This is `NULL` for empty [BenchmarkResult]s.
    task_type = function(rhs) {
      assert_ro_binding(rhs)
      if (nrow(self$data$fact) == 0L) {
        return(NULL)
      }
      self$data$task_objs$task[[1L]]$task_type
    },

    #' @field tasks ([data.table::data.table()])\cr
    #' Table of included [Task]s with three columns:
    #'
    #' * `"task_hash"` (`character(1)`),
    #' * `"task_id"` (`character(1)`), and
    #' * `"task"` ([Task]).
    tasks = function(rhs) {
      assert_ro_binding(rhs)
      tab = self$data$tasks[self$data$task_objs, on = "task_phash"]
      set(tab, j = "task", value = reassemble_tasks(tasks = tab$task, feature_names = tab$feature_names))
      tab[, c("task_hash", "task_id", "task"), with = FALSE]
    },

    #' @field learners ([data.table::data.table()])\cr
    #' Table of included [Learner]s with three columns:
    #'
    #' * `"learner_hash"` (`character(1)`),
    #' * `"learner_id"` (`character(1)`), and
    #' * `"learner"` ([Learner]).
    #'
    #' Note that it is not feasible to access learned models via this field, as the training task would be ambiguous.
    #' For this reason the returned learner are reseted before they are returned.
    #' Instead, select a row from the table returned by `$score()`.
    learners = function(rhs) {
      assert_ro_binding(rhs)
      tab = self$data$learners[self$data$learner_objs, on = "learner_phash"]
      set(tab, j = "learner", value = reassemble_learners(learners = tab$learner, param_vals = tab$param_vals))
      tab[, c("learner_hash", "learner_id", "learner"), with = FALSE]
    },

    #' @field resamplings ([data.table::data.table()])\cr
    #' Table of included [Resampling]s with three columns:
    #'
    #' * `"resampling_hash"` (`character(1)`),
    #' * `"resampling_id"` (`character(1)`), and
    #' * `"resampling"` ([Resampling]).
    resamplings = function(rhs) {
      assert_ro_binding(rhs)
      tab = copy(self$data$resamplings)
      setcolorder(tab, c("resampling_hash", "resampling_id", "resampling"))[]
    },

    #' @field resample_results ([data.table::data.table()])\cr
    #' Returns a table with three columns:
    #' * `uhash` (`character()`).
    #' * `iters` (`integer()`).
    #' * `resample_result` ([ResampleResult]).
    resample_results = function() {
      uhashes = self$uhashes
      rrs = lapply(uhashes, function(uhash) snowflake_rr_result(self$data, uhash))
      data.table(
        uhash = uhashes,
        iters = map_int(rrs, function(rr) nrow(rr$data$fact)),
        resample_result = rrs
      )
    },

    #' @field n_resample_results (`integer(1)`)\cr
    #' Returns the total number of stored [ResampleResult]s.
    n_resample_results = function(rhs) {
      assert_ro_binding(rhs)
      nrow(self$data$uhash)
    },

    #' @field uhashes (`character()`)\cr
    #' Set of (unique) hashes of all included [ResampleResult]s.
    uhashes = function(rhs) {
      assert_ro_binding(rhs)
      self$data$uhash$uhash
    }
  ),

  private = list(
    deep_clone = function(name, value) {
      if (name %in% "data") {
        snowflake_copy(value)
      } else {
        value
      }
    }
  )
)

#' @export
as.data.table.BenchmarkResult = function(x, ..., hashes = FALSE, predict_sets = "test") { # nolint
  as.data.table(x$data, hashes = FALSE, predict_sets = predict_sets)
}

#' @export
c.BenchmarkResult = function(...) { # nolint
  bmrs = lapply(list(...), as_benchmark_result)
  init = BenchmarkResult$new()
  Reduce(function(lhs, rhs) lhs$combine(rhs), bmrs, init = init)
}

#' @importFrom stats friedman.test
#' @export
friedman.test.BenchmarkResult = function(y, measure = NULL, ...) { # nolint
  # FIXME: this must be documented somewhere else
  measure = assert_measure(as_measure(measure, task_type = y$task_type))
  aggr = y$aggregate(measure)
  friedman.test(aggr[[measure$id]], aggr$learner_id, aggr$task_id)
}

#' @title Convert to BenchmarkResult
#'
#' @description
#' Simple S3 method to convert objects to a [BenchmarkResult].
#'
#' @param x (`any`)\cr
#'  Object to dispatch on, e.g. a [ResampleResult].
#' @param ... (`any`)\cr
#'  Currently not used.
#'
#' @return ([BenchmarkResult]).
#' @export
as_benchmark_result = function(x, ...) {
  UseMethod("as_benchmark_result")
}

#' @export
as_benchmark_result.BenchmarkResult = function(x, ...) { # nolint
  x
}

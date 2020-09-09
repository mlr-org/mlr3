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
#' * `as.data.table(bmr)`\cr
#'   [BenchmarkResult] -> [data.table::data.table()]\cr
#'   Returns a copy of the internal data.
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

    #' @field rr_data ([data.table::data.table()])\cr
    #'   Internal data storage with one row per [ResampleResult]
    #'   (instead of one row per resampling iteration as in `$data`).
    #'
    #'   Package develops may opt to add additional columns here.
    #'   These columns are preserved in all mutators.
    #'
    #'   Can be combined with `$data` by (left) joining on the key column `"uhash"`.
    rr_data = NULL,

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
    #'   * `"iteration"` (`integer(1)`),
    #'   * `"prediction"` ([PredictionData]), and
    #'   * `"uhash"` (`character(1)`).
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

      private$.tasks = normalize_tab(data, "task")
      private$.learners = normalize_tab(data, "learner")
      private$.resamplings = normalize_tab(data, "resampling")

      setcolorder(data, slots)[]
      self$data = data
      self$rr_data = data[, list(uhash = unique(uhash))]
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
        format(self), nrow(self$data), nrow(tab))
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
        if (nrow(self$data) && self$task_type != bmr$task_type) {
          stopf("BenchmarkResult is of task type '%s', but must be '%s'", bmr$task_type, self$task_type)
        }

        self$data = rbindlist(list(self$data, bmr$data), fill = TRUE, use.names = TRUE)
        self$rr_data = unique(rbindlist(list(self$rr_data, bmr$rr_data), fill = TRUE, use.names = TRUE), by = "uhash")

        pbmr = get_private(bmr)
        insert_named(private$.tasks, pbmr$.tasks)
        insert_named(private$.learners, pbmr$.learners)
        insert_named(private$.resamplings, pbmr$.resamplings)
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
    #' @return [data.table::data.table()].
    score = function(measures = NULL, ids = TRUE) {
      measures = assert_measures(as_measures(measures, task_type = self$task_type))
      assert_flag(ids)

      tab = score_measures(self, measures)

      tab[, ("nr") := .GRP, by = "uhash"][, ("uhash") := NULL]
      if (ids) {
        tab[, "task_id" := ids(task)]
        tab[, "learner_id" := ids(learner)]
        tab[, "resampling_id" := ids(resampling)]
        setcolorder(tab, c("nr", "task", "task_id", "learner", "learner_id",
            "resampling", "resampling_id", "iteration", "prediction"))
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
      res = self$resample_results
      resample_result = res$resample_result
      res[, "nr" := seq_row(res)]

      if (assert_flag(ids)) {
        res[, "task_id" := map_chr(resample_result, function(rr) rr$task$id)]
        res[, "learner_id" := map_chr(resample_result, function(rr) rr$learner$id)]
        res[, "resampling_id" := map_chr(resample_result, function(rr) rr$resampling$id)]
      }

      # move iters to last column
      setcolorder(res, setdiff(names(res), "iters"))

      if (assert_flag(params)) {
        res[, "params" := list(map(resample_result, function(x) x$learner$param_set$values))]
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

      keep_ids = function(ee, ids) {
        delete = names(ee)[ids(ee) %nin% ids]
        rm(list = delete, envir = ee)
      }

      keep_hashes = function(ee, hashes) {
        delete = setdiff(names(ee), hashes)
        rm(list = delete, envir = ee)
      }

      if (!is.null(task_ids)) {
        assert_character(task_ids, any.missing = FALSE)
        keep_ids(private$.tasks, task_ids)
      }

      if (!is.null(task_hashes)) {
        assert_character(task_hashes, any.missing = FALSE)
        keep_hashes(private$.tasks, task_hashes)
      }

      if (!is.null(learner_ids)) {
        assert_character(learner_ids, any.missing = FALSE)
        keep_ids(private$.learners, learner_ids)
      }

      if (!is.null(learner_hashes)) {
        assert_character(learner_hashes, any.missing = FALSE)
        keep_hashes(private$.learners, learner_hashes)
      }

      if (!is.null(resampling_ids)) {
        assert_character(resampling_ids, any.missing = FALSE)
        keep_ids(private$.resamplings, resampling_ids)
      }

      if (!is.null(resampling_hashes)) {
        assert_character(resampling_hashes, any.missing = FALSE)
        keep_hashes(private$.resamplings, resampling_hashes)
      }

      self$data = self$data[task %in% names(private$.tasks)]
      self$data = self$data[learner %in% names(private$.learners)]
      self$data = self$data[resampling %in% names(private$.resamplings)]
      self$rr_data = self$rr_data[uhash %in% self$data$uhash]

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

      rrs = bmr_resample_results(self, self$data[list(needle), on = "uhash"])
      rrs$resample_result[[1L]]
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
      if (nrow(self$data) == 0L) {
        return(NULL)
      }
      self$tasks$task[[1L]]$task_type
    },

    #' @field tasks ([data.table::data.table()])\cr
    #' Table of included [Task]s with three columns:
    #'
    #' * `"task_hash"` (`character(1)`),
    #' * `"task_id"` (`character(1)`), and
    #' * `"task"` ([Task]).
    tasks = function(rhs) {
      assert_ro_binding(rhs)
      env2tab(private$.tasks, "task")
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
      env2tab(private$.learners, "learner")
    },

    #' @field resamplings ([data.table::data.table()])\cr
    #' Table of included [Resampling]s with three columns:
    #'
    #' * `"resampling_hash"` (`character(1)`),
    #' * `"resampling_id"` (`character(1)`), and
    #' * `"resampling"` ([Resampling]).
    resamplings = function(rhs) {
      assert_ro_binding(rhs)
      env2tab(private$.resamplings, "resampling")
    },

    #' @field resample_results ([data.table::data.table()])\cr
    #' Returns a table with three columns:
    #' * `uhash` (`character()`).
    #' * `iters` (`integer()`).
    #' * `resample_result` ([ResampleResult]).
    resample_results = function() {
      bmr_resample_results(self)
    },

    #' @field n_resample_results (`integer(1)`)\cr
    #' Returns the total number of stored [ResampleResult]s.
    n_resample_results = function(rhs) {
      assert_ro_binding(rhs)
      nrow(self$rr_data)
    },

    #' @field uhashes (`character()`)\cr
    #' Set of (unique) hashes of all included [ResampleResult]s.
    uhashes = function(rhs) {
      assert_ro_binding(rhs)
      self$rr_data$uhash
    }
  ),

  private = list(
    .tasks = NULL,
    .learners = NULL,
    .resamplings = NULL,

    deep_clone = function(name, value) {
      if (name %in% c(".tasks", ".learners", ".resamplings")) {
        copy_r6_dict(value, clone = FALSE)
      } else if (name %in% c("data", "rr_data")) {
        copy(value)
      } else {
        value
      }
    }
  )
)

#' @export
as.data.table.BenchmarkResult = function(x, ..., reassemble_learners = TRUE, convert_predictions = TRUE, predict_sets = "test") { # nolint
  assert_flag(reassemble_learners)
  assert_flag(convert_predictions)

  denormalize_tab(x, reassemble_learners = reassemble_learners,
    convert_predictions = convert_predictions, predict_sets = predict_sets)
}

#' @export
c.BenchmarkResult = function(...) { # nolint
  bmrs = lapply(list(...), as_benchmark_result)
  Reduce(function(lhs, rhs) lhs$combine(rhs), tail(bmrs, -1L), init = bmrs[[1L]]$clone(deep = TRUE))
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

bmr_resample_results = function(bmr, data = bmr$data) {
  private = get_private(bmr)
  task = learner = resampling = state = iteration = prediction = uhash = NULL

  data[, list(
    iters = .N,
    resample_result = list(if (.N == 0L) NULL else ResampleResult$new(
      task = private$.tasks[[task[1L]]],
      learner = private$.learners[[learner[1L]]],
      resampling = private$.resamplings[[resampling[1L]]],
      states = state, iterations = iteration,
      predictions = prediction, uhash = uhash[1L]
    ))
  ), by = "uhash"]
}

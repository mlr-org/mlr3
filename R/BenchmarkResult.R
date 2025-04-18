#' @title Container for Benchmarking Results
#'
#' @include mlr_reflections.R
#'
#' @description
#' This is the result container object returned by [benchmark()].
#' A [BenchmarkResult] consists of the data of multiple [ResampleResult]s.
#' The contents of a `BenchmarkResult` and [ResampleResult] are almost identical and the stored [ResampleResult]s can be extracted via the `$resample_result(i)` method, where i is the index of the performed resample experiment.
#' This allows us to investigate the extracted [ResampleResult] and individual resampling iterations, as well as the predictions and models from each fold.
#'
#' [BenchmarkResult]s can be visualized via \CRANpkg{mlr3viz}'s `autoplot()` function.
#'
#' For statistical analysis of benchmark results and more advanced plots, see \CRANpkg{mlr3benchmark}.
#'
#' @note
#' All stored objects are accessed by reference.
#' Do not modify any extracted object without cloning it first.
#'
#' @template param_measures
#'
#' @section S3 Methods:
#' * `as.data.table(rr, ..., reassemble_learners = TRUE, convert_predictions = TRUE, predict_sets = "test", task_characteristics = FALSE)`\cr
#'   [BenchmarkResult] -> [data.table::data.table()]\cr
#'   Returns a tabular view of the internal data.
#' * `c(...)`\cr
#'   ([BenchmarkResult], ...) -> [BenchmarkResult]\cr
#'   Combines multiple objects convertible to [BenchmarkResult] into a new [BenchmarkResult].
#'
#' @template seealso_benchmark
#' @export
#' @examples
#' set.seed(123)
#' learners = list(
#'   lrn("classif.featureless", predict_type = "prob"),
#'   lrn("classif.rpart", predict_type = "prob")
#' )
#'
#' design = benchmark_grid(
#'   tasks = list(tsk("sonar"), tsk("penguins")),
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
#' # first 5 resampling iterations
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
#'
#' # reduce to subset with task id "sonar"
#' bmr$filter(task_ids = "sonar")
#' print(bmr)
BenchmarkResult = R6Class("BenchmarkResult",
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param data (`ResultData`)\cr
    #'   An object of type `ResultData`, either extracted from another [ResampleResult], another
    #'   [BenchmarkResult], or manually constructed with [as_result_data()].
    initialize = function(data = NULL) {
      if (is.null(data)) {
        private$.data = ResultData$new()
      } else {
        private$.data = assert_class(data, "ResultData")
      }
    },

    #' @description
    #' Opens the help page for this object.
    help = function() {
      open_help("mlr3::BenchmarkResult")
    },

    #' @description
    #' Helper for print outputs.
    #' @param ... (ignored).
    format = function(...) {
      sprintf("<%s>", class(self)[1L])
    },

    #' @description
    #' Printer.
    print = function() {
      tab = self$aggregate(measures = list(), conditions = TRUE)
      setattr(tab, "class", c("data.table", "data.frame"))
      catf("%s of %i rows with %i resampling runs",
        format(self), private$.data$iterations(), nrow(tab))
      if (nrow(tab)) {
        tab = remove_named(tab, c("uhash", "resample_result"))
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
    #' You need to explicitly `$clone()` the object beforehand if you want to keep
    #' the object in its previous state.
    combine = function(bmr) {
      if (!is.null(bmr)) {
        assert_benchmark_result(bmr)
        if (private$.data$iterations() && self$task_type != bmr$task_type) {
          stopf("BenchmarkResult is of task type '%s', but must be '%s'", bmr$task_type, self$task_type)
        }

        private$.data$combine(get_private(bmr)$.data)
      }

      invisible(self)
    },

    #' @description
    #' Marshals all stored models.
    #' @param ... (any)\cr
    #'   Additional arguments passed to [`marshal_model()`].
    marshal = function(...) {
      private$.data$marshal(...)
    },
    #' @description
    #' Unmarshals all stored models.
    #' @param ... (any)\cr
    #'   Additional arguments passed to [`unmarshal_model()`].
    unmarshal = function(...) {
      private$.data$unmarshal(...)
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
    #'   extra character columns to the returned table.
    #'
    #' @param conditions (`logical(1)`)\cr
    #'   Adds condition messages (`"warnings"`, `"errors"`) as extra
    #'   list columns of character vectors to the returned table
    #'
    #' @param predictions (`logical(1)`)\cr
    #'   Additionally return prediction objects, one column for each `predict_set` of all learners combined.
    #'   Columns are named `"prediction_train"`, `"prediction_test"` and `"prediction_internal_valid"`,
    #'   if present.
    #'
    #' @return [data.table::data.table()].
    score = function(measures = NULL, ids = TRUE, conditions = FALSE, predictions = TRUE) {
      measures = assert_measures(as_measures(measures, task_type = self$task_type))
      assert_flag(ids)
      assert_flag(conditions)
      assert_flag(predictions)

      tab = score_measures(self, measures, view = NULL)
      tab = merge(private$.data$data$uhashes, tab, by = "uhash", sort = FALSE)
      tab[, "nr" := .GRP, by = "uhash"]

      if (ids) {
        set(tab, j = "task_id", value = ids(tab$task))
        set(tab, j = "learner_id", value = ids(tab$learner))
        set(tab, j = "resampling_id", value = ids(tab$resampling))
      }

      if (conditions) {
        set(tab, j = "warnings", value = map(tab$learner, "warnings"))
        set(tab, j = "errors", value = map(tab$learner, "errors"))
      }

      if (predictions && nrow(tab)) {
        predict_sets = intersect(
          mlr_reflections$predict_sets,
          unlist(map(self$learners$learner, "predict_sets"), use.names = FALSE)
        )
        predict_cols = sprintf("prediction_%s", predict_sets)
        for (i in seq_along(predict_sets)) {
          set(tab, j = predict_cols[i],
            value = map(tab$prediction, function(p) as_prediction(p[[predict_sets[i]]], check = FALSE))
          )
        }
      } else {
        predict_cols = character()
      }

      set_data_table_class(tab, "bmr_score")

      cns = c("uhash", "nr", "task", "task_id", "learner", "learner_id", "resampling", "resampling_id",
        "iteration", predict_cols, "warnings", "errors", ids(measures))
      cns = intersect(cns, names(tab))
      tab[, cns, with = FALSE]
    },

    #' @description
    #' Calculates the observation-wise loss via the loss function set in the
    #' [Measure]'s field `obs_loss`.
    #' Returns a `data.table()` with the columns `row_ids`, `truth`, `response` and
    #' one additional numeric column for each measure, named with the respective measure id.
    #' If there is no observation-wise loss function for the measure, the column is filled with
    #' `NA` values.
    #' Note that some measures such as RMSE, do have an `$obs_loss`, but they require an
    #' additional transformation after aggregation, in this example taking the square-root.
    #' @param predict_sets (`character()`)\cr
    #'   The predict sets.
    obs_loss = function(measures = NULL, predict_sets = "test") {
      measures = assert_measures(as_measures(measures, task_type = self$task_type))
      map_dtr(self$resample_results$resample_result,
        function(rr) {
          rr$obs_loss(measures, predict_sets)
        }, .idcol = "resample_result")
    },

    #' @description
    #' Returns a result table where resampling iterations are combined into
    #' [ResampleResult]s. A column with the aggregated performance score is
    #' added for each [Measure], named with the id of the respective measure.
    #'
    #' The method for aggregation is controlled by the [Measure], e.g. micro
    #' aggregation, macro aggregation or custom aggregation. Most measures
    #' default to macro aggregation.
    #'
    #' Note that the aggregated performances just give a quick impression which
    #' approaches work well and which approaches are probably underperforming.
    #' However, the aggregates do not account for variance and cannot replace
    #' a statistical test.
    #' See \CRANpkg{mlr3viz} to get a better impression via boxplots or
    #' \CRANpkg{mlr3benchmark} for critical difference plots and
    #' significance tests.
    #'
    #' For convenience, different flags can be set to extract more
    #' information from the returned [ResampleResult].
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
      measures = if (is.null(measures)) {
        default_measures(self$task_type)
      } else {
        assert_measures(as_measures(measures))
      }
      assert_flag(ids)
      assert_flag(uhashes)
      assert_flag(params)
      assert_flag(conditions)

      create_rr = function(view) {
        if (length(view)) ResampleResult$new(private$.data, view = copy(view)) else list()
      }

      rdata = private$.data$data
      tab = rdata$fact[rdata$uhashes, list(
        nr = .GRP,
        iters = .N,
        task_hash = .SD$task_hash[1L],
        learner_hash = .SD$learner_hash[1L],
        learner_phash = .SD$learner_phash[1L],
        resampling_hash = .SD$resampling_hash[1L],
        resample_result = list(create_rr(.BY[[1L]])),
        warnings = if (conditions) sum(map_int(.SD$learner_state, function(s) sum(s$log$class == "warning"))) else NA_integer_,
        errors = if (conditions) sum(map_int(.SD$learner_state, function(s) sum(s$log$class == "error"))) else NA_integer_
      ), by = "uhash", on = "uhash", nomatch = NULL]

      if (ids) {
        tab = merge(tab, rdata$tasks[, list(task_hash = .SD$task_hash, task_id = ids(.SD$task))],
          by = "task_hash", sort = FALSE)
        tab = merge(tab, rdata$learners[, list(learner_phash = .SD$learner_phash, learner_id = ids(.SD$learner))],
          by = "learner_phash", sort = FALSE)
        tab = merge(tab, rdata$resamplings[, list(resampling_hash = .SD$resampling_hash, resampling_id = ids(.SD$resampling))],
          by = "resampling_hash", sort = FALSE)
      }

      if (!uhashes) {
        set(tab, j = "uhash", value = NULL)
      }

      if (params) {
        tab = merge(tab, rdata$learner_components, by = "learner_hash", sort = FALSE)
        setnames(tab, "learner_param_vals", "params")
      }

      if (!conditions) {
        tab = remove_named(tab, c("warnings", "errors"))
      }

      if (nrow(tab) > 0L) {
        scores = map_dtr(tab$resample_result, function(rr) as.list(resample_result_aggregate(rr, measures)))
      } else {
        scores = setDT(named_list(ids(measures), double()))
      }
      tab = insert_named(tab, scores)

      set_data_table_class(tab, "bmr_aggregate")

      cns = c("uhash", "nr", "resample_result", "task_id", "learner_id", "resampling_id", "iters",
        "warnings", "errors", "params", names(scores))
      cns = intersect(cns, names(tab))
      tab[, cns, with = FALSE]
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
      learner_phashes = NULL

      filter_if_not_null = function(column, hashes) {
        if (is.null(hashes)) {
          fact
        } else {
          fact[unique(hashes), on = column, nomatch = NULL]
        }
      }


      if (!is.null(task_ids)) {
        task = task_hash = NULL
        task_hashes = union(task_hashes, private$.data$data$tasks[ids(task) %in% task_ids, task_hash])
      }

      if (!is.null(learner_ids)) {
        learner = learner_phash = NULL
        learner_phashes = private$.data$data$learners[ids(learner) %in% learner_ids, learner_phash]
      }

      if (!is.null(resampling_ids)) {
        resampling = resampling_hash = NULL
        resampling_hashes = union(resampling_hashes, private$.data$data$resamplings[ids(resampling) %in% resampling_ids, resampling_hash])
      }

      fact = private$.data$data$fact
      fact = filter_if_not_null("task_hash", task_hashes)
      fact = filter_if_not_null("learner_hash", learner_hashes)
      fact = filter_if_not_null("learner_phash", learner_phashes)
      fact = filter_if_not_null("resampling_hash", resampling_hashes)

      private$.data$data$fact = fact
      private$.data$sweep()

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

      uhashes = private$.data$uhashes()
      if (is.null(i)) {
        needle = assert_choice(uhash, uhashes)
      } else {
        i = assert_int(i, lower = 1L, upper = length(uhashes), coerce = TRUE)
        needle = uhashes[i]
      }

      ResampleResult$new(private$.data, view = needle)
    },

    #' @description
    #' Shrinks the [BenchmarkResult] by discarding parts of the internally stored data.
    #' Note that certain operations might stop work, e.g. extracting
    #' importance values from learners or calculating measures requiring the task's data.
    #'
    #' @param backends (`logical(1)`)\cr
    #'   If `TRUE`, the [DataBackend] is removed from all stored [Task]s.
    #' @param models (`logical(1)`)\cr
    #'   If `TRUE`, the stored model is removed from all [Learner]s.
    #'
    #' @return
    #' Returns the object itself, but modified **by reference**.
    #' You need to explicitly `$clone()` the object beforehand if you want to keeps
    #' the object in its previous state.
    discard = function(backends = FALSE, models = FALSE) {
      private$.data$discard(backends = backends, models = models)
    }
  ),

  active = list(
    #' @field task_type (`character(1)`)\cr
    #' Task type of objects in the `BenchmarkResult`.
    #' All stored objects ([Task], [Learner], [Prediction]) in a single `BenchmarkResult` are
    #' required to have the same task type, e.g., `"classif"` or `"regr"`.
    #' This is `NA` for empty [BenchmarkResult]s.
    task_type = function(rhs) {
      assert_ro_binding(rhs)
      private$.data$task_type
    },

    #' @field tasks ([data.table::data.table()])\cr
    #' Table of included [Task]s with three columns:
    #'
    #' * `"task_hash"` (`character(1)`),
    #' * `"task_id"` (`character(1)`), and
    #' * `"task"` ([Task]).
    tasks = function(rhs) {
      assert_ro_binding(rhs)

      tab = private$.data$tasks()
      set(tab, j = "task_id", value = ids(tab$task))
      setcolorder(tab, c("task_hash", "task_id", "task"))[]
    },

    #' @field learners ([data.table::data.table()])\cr
    #' Table of included [Learner]s with three columns:
    #'
    #' * `"learner_hash"` (`character(1)`),
    #' * `"learner_id"` (`character(1)`), and
    #' * `"learner"` ([Learner]).
    #'
    #' Note that it is not feasible to access learned models via this field, as the training task would be ambiguous.
    #' For this reason the returned learner are reset before they are returned.
    #' Instead, select a row from the table returned by `$score()`.
    learners = function(rhs) {
      assert_ro_binding(rhs)

      tab = private$.data$learners(states = FALSE)
      set(tab, j = "learner_id", value = ids(tab$learner))
      setcolorder(tab, c("learner_hash", "learner_id", "learner"))[]
    },

    #' @field resamplings ([data.table::data.table()])\cr
    #' Table of included [Resampling]s with three columns:
    #'
    #' * `"resampling_hash"` (`character(1)`),
    #' * `"resampling_id"` (`character(1)`), and
    #' * `"resampling"` ([Resampling]).
    resamplings = function(rhs) {
      assert_ro_binding(rhs)

      tab = private$.data$resamplings()
      set(tab, j = "resampling_id", value = ids(tab$resampling))
      setcolorder(tab, c("resampling_hash", "resampling_id", "resampling"))[]
    },

    #' @field resample_results ([data.table::data.table()])\cr
    #' Returns a table with three columns:
    #' * `uhash` (`character()`).
    #' * `resample_result` ([ResampleResult]).
    resample_results = function(rhs) {
      assert_ro_binding(rhs)
      rdata = private$.data$data

      create_rr = function(view) {
        if (length(view)) ResampleResult$new(private$.data, view = copy(view)) else list()
      }
      rdata$fact[rdata$uhashes, list(
        nr = .GRP,
        resample_result = list(create_rr(.BY[[1L]]))
      ), by = "uhash"]
    },

    #' @field n_resample_results (`integer(1)`)\cr
    #' Returns the total number of stored [ResampleResult]s.
    n_resample_results = function(rhs) {
      assert_ro_binding(rhs)
      length(private$.data$uhashes())
    },

    #' @field uhashes (`character()`)\cr
    #' Set of (unique) hashes of all included [ResampleResult]s.
    uhashes = function(rhs) {
      assert_ro_binding(rhs)
      private$.data$uhashes()
    }
  ),

  private = list(
    # @field data (`ResultData`)\cr
    # Internal data storage object of type `ResultData`.
    .data = NULL,

    deep_clone = function(name, value) {
      if (name == ".data") value$clone(deep = TRUE) else value
    }
  )
)

#' @export
as.data.table.BenchmarkResult = function(x, ..., hashes = FALSE, predict_sets = "test", task_characteristics = FALSE) { # nolint
  assert_flag(task_characteristics)
  tab = get_private(x)$.data$as_data_table(view = NULL, predict_sets = predict_sets)
  cns = c("uhash", "task", "learner", "resampling", "iteration", "prediction",
    if ("data_extra" %in% names(tab)) "data_extra")
  tab = tab[, cns, with = FALSE]

  set(tab, j = "task_id", value = ids(tab$task))
  set(tab, j = "learner_id", value = ids(tab$learner))
  set(tab, j = "resampling_id", value = ids(tab$resampling))

  if (task_characteristics) {
    set(tab, j = "characteristics", value = map(tab$task, "characteristics"))
    tab = unnest(tab, "characteristics")
  }

  tab[]
}

#' @export
c.BenchmarkResult = function(...) { # nolint
  bmrs = lapply(list(...), as_benchmark_result)
  init = BenchmarkResult$new()
  Reduce(function(lhs, rhs) lhs$combine(rhs), bmrs, init = init)
}

#' @export
print.bmr_score = function(x, ...) {
  print_data_table(x, c("uhash", "task", "learner", "resampling", "prediction"))
}

#' @export
print.bmr_aggregate = function(x, ...) {
  print_data_table(x, "resample_result")
}

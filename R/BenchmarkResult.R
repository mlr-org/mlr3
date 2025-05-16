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
      cat_cli(cli_h1("{.cls {class(self)[1L]}} of {.val {private$.data$iterations()}} rows with {.val {nrow(tab)}} resampling run"))
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
    #' Subsets the benchmark result.
    #' You can either directly provide the row IDs or the uhashes of the resample results to keep,
    #' or use the `learner_ids`, `task_ids` and `resampling_ids` arguments to filter for learner, task and resampling IDs.
    #' The three options are mutually exclusive.
    #'
    #' @param i (`integer()` | `NULL`)\cr
    #'   The iteration values to filter for.
    #' @param uhashes (`character()` | `NULL`)\cr
    #'   The uhashes of the resample results to filter for.
    #' @param learner_ids (`character()` | `NULL`)\cr
    #'   The learner IDs to filter for.
    #' @param task_ids (`character()` | `NULL`)\cr
    #'   The task IDs to filter for.
    #' @param resampling_ids (`character()` | `NULL`)\cr
    #'   The resampling IDs to filter for.
    #'
    #' @return
    #' Returns the object itself, but modified **by reference**.
    #' You need to explicitly `$clone()` the object beforehand if you want to keeps
    #' the object in its previous state.
    #' @examples
    #' design = benchmark_grid(
    #'   tsks(c("iris", "sonar")),
    #'   lrns(c("classif.debug", "classif.featureless")),
    #'   rsmp("holdout")
    #' )
    #' bmr = benchmark(design)
    #' bmr
    #' bmr2 = bmr$clone(deep = TRUE)
    #' bmr2$filter(learner_ids = "classif.featureless")
    #' bmr2
    filter = function(i = NULL, uhashes = NULL, learner_ids = NULL, task_ids = NULL, resampling_ids = NULL) {
      uhashes = private$.get_uhashes(i, uhashes, learner_ids, task_ids, resampling_ids)

      fact = private$.data$data$fact
      fact = if (is.null(uhashes)) {
        fact
      } else {
        fact[unique(uhashes), on = "uhash", nomatch = NULL]
      }

      setkeyv(fact, c("uhash", "iteration"))
      private$.data$data$fact = fact
      private$.data$sweep()

      invisible(self)
    },

    #' @description
    #' Retrieve the i-th [ResampleResult], by position, by unique hash `uhash` or by learner,
    #' task and resampling IDs.
    #' All three options are mutually exclusive.
    #'
    #' @param i (`integer(1)` | `NULL`)\cr
    #'   The iteration value to filter for.
    #' @param uhash (`character(1)` | `NULL`)\cr
    #'   The unique identifier to filter for.
    #' @param learner_id (`character(1)` | `NULL`)\cr
    #'   The learner ID to filter for.
    #' @param task_id (`character(1)` | `NULL`)\cr
    #'   The task ID to filter for.
    #' @param resampling_id (`character(1)` | `NULL`)\cr
    #'   The resampling ID to filter for.
    #'
    #' @examples
    #' design = benchmark_grid(
    #'   tsk("iris"),
    #'   lrns(c("classif.debug", "classif.featureless")),
    #'   rsmp("holdout")
    #' )
    #' bmr = benchmark(design)
    #' bmr$resample_result(learner_id = "classif.featureless")
    #' bmr$resample_result(i = 1)
    #' bmr$resample_result(uhash = uhashes(bmr, learner_id = "classif.debug"))
    #' @return [ResampleResult].
    resample_result = function(i = NULL, uhash = NULL, task_id = NULL, learner_id = NULL,
      resampling_id = NULL) {
      uhash = private$.get_uhashes(i, uhash, learner_id, task_id, resampling_id)
      if (length(uhash) != 1) {
        stopf("Method requires selecting exactly one ResampleResult, but got %s",
          length(uhash))
      }
      ResampleResult$new(private$.data, view = uhash)
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
    },

    #' @description
    #' Sets the threshold for the response prediction of classification learners, given they have
    #' output a probability prediction for a binary classification task.
    #'
    #' The resample results for which to change the threshold can either be specified directly
    #' via `uhashes`, by selecting the specific iterations (`i`) or by filtering according to
    #' learner, task and resampling IDs.
    #'
    #' If none of the three options is specified, the threshold is set for all resample results.
    #'
    #' @param threshold (`numeric(1)`)\cr
    #'   Threshold value.
    #' @param i (`integer()` | `NULL`)\cr
    #'   The iteration values to filter for.
    #' @param uhashes (`character()` | `NULL`)\cr
    #'   The unique identifiers of the [ResampleResult]s for which the threshold should be set.
    #' @param learner_ids (`character()` | `NULL`)\cr
    #'   The learner IDs for which the threshold should be set.
    #' @param task_ids (`character()` | `NULL`)\cr
    #'   The task IDs for which the threshold should be set.
    #' @param resampling_ids (`character()` | `NULL`)\cr
    #'   The resampling IDs for which the threshold should be set.
    #' @template param_ties_method
    #' @examples
    #' design = benchmark_grid(
    #'   tsk("sonar"),
    #'   lrns(c("classif.debug", "classif.featureless"), predict_type = "prob"),
    #'   rsmp("holdout")
    #' )
    #' bmr = benchmark(design)
    #' bmr$set_threshold(0.8, learner_ids = "classif.featureless")
    #' bmr$set_threshold(0.3, i = 2)
    #' bmr$set_threshold(0.7, uhashes = uhashes(bmr, learner_ids = "classif.featureless"))
    set_threshold = function(threshold, i = NULL, uhashes = NULL, learner_ids = NULL, task_ids = NULL,
      resampling_ids = NULL, ties_method = "random") {
      uhashes = private$.get_uhashes(i, uhashes, learner_ids, task_ids, resampling_ids)
      private$.data$set_threshold(uhashes, threshold, ties_method)
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
    },

    #' @field uhash_table ([data.table::data.table])\cr
    #' Table with columns `uhash`, `learner_id`, `task_id` and `resampling_id`.
    uhash_table = function(rhs) {
      assert_ro_binding(rhs)
      private$.data$uhash_table()
    }
  ),

  private = list(
    # @field data (`ResultData`)\cr
    # Internal data storage object of type `ResultData`.
    .data = NULL,

    .get_uhashes = function(i, uhashes, learner_ids, task_ids, resampling_ids) {
      args = discard(list(learner_ids = learner_ids, task_ids = task_ids,
        resampling_ids = resampling_ids), is.null)

      if (sum(!is.null(i), !is.null(uhashes), length(args) > 0L) > 1) {
        stopf("At most one of `i`, `uhash`, or IDs can be provided.")
      }
      if (!is.null(i)) {
        uhashes = self$uhashes
        uhashes = uhashes[assert_integerish(i, lower = 1L, upper = length(uhashes))]
      } else if (!is.null(uhashes)) {
        assert_subset(uhashes, self$uhashes)
      } else {
        uhashes = invoke(match.fun("uhashes"), bmr = self, .args = args)
      }
      if (length(uhashes) == 0L) {
        stopf("No resample results found for the given arguments.")
      }
      uhashes
    },

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

#' @param learner_ids (`character()` | `NULL`)\cr
#'   Learner IDs.
#' @param task_ids (`character()` | `NULL`)\cr
#'   Task IDs.
#' @param resampling_ids (`character()` | `NULL`)\cr
#'   Resampling IDs.
#' @rdname uhash
#' @export
uhashes = function(bmr, learner_ids = NULL, task_ids = NULL, resampling_ids = NULL) {
  assert_class(bmr, "BenchmarkResult")
  assert_character(learner_ids, null.ok = TRUE)
  assert_character(task_ids, null.ok = TRUE)
  assert_character(resampling_ids, null.ok = TRUE)
  bmr$uhash_table[
    (is.null(learner_ids) | (get("learner_id") %in% learner_ids)) &
      (is.null(task_ids) | (get("task_id") %in% task_ids)) &
      (is.null(resampling_ids) | (get("resampling_id") %in% resampling_ids)), "uhash"
  ]$uhash
}

#' @title Obtain specific uhashes from a [BenchmarkResult]
#' @description
#' In a [`BenchmarkResult`], each [ResampleResult] is *u*niquely identified by a *hash* (*uhash*).
#' Operations that select specific [ResampleResult]s from a [BenchmarkResult] operate using
#' these hashes.
#' This function allows to obtain uhashes for specific learners, tasks, and resamplings.
#'
#' If you want more control, you can also directly obtain the uhash table from the [`BenchmarkResult`]
#' via the field `$uhash_table`.
#'
#' @param bmr (`BenchmarkResult`)\cr
#'   Benchmark result.
#' @param learner_id (`character(1)` | `NULL`)\cr
#'   Learner ID.
#' @param task_id (`character(1)` | `NULL`)\cr
#'   Task ID.
#' @param resampling_id (`character(1)` | `NULL`)\cr
#'   Resampling ID.
#' @export
#' @examples
#' design = benchmark_grid(
#'   tsks(c("sonar", "iris")),
#'   lrns(c("classif.debug", "classif.featureless", "classif.rpart")),
#'   rsmp("holdout")
#' )
#' bmr = benchmark(design)
#' bmr
#' bmr$uhashes
#' uhash(bmr, learner_id = "classif.debug", task_id = "sonar", resampling_id = "holdout")
#' uhashes(bmr, learner_ids = c("classif.debug", "classif.featureless"))
uhash = function(bmr, learner_id = NULL, task_id = NULL, resampling_id = NULL) {
  assert_string(learner_id, null.ok = TRUE)
  assert_string(task_id, null.ok = TRUE)
  assert_string(resampling_id, null.ok = TRUE)
  uhash = uhashes(bmr, learner_id, task_id, resampling_id)
  if (length(uhash) != 1) {
    stopf("Expected exactly one uhash, got %s", length(uhash))
  }
  uhash
}

#' @title Container for Results of `resample()`
#'
#' @include mlr_reflections.R
#'
#' @description
#' This is the result container object returned by [resample()].
#'
#' Note that all stored objects are accessed by reference.
#' Do not modify any object without cloning it first.
#'
#' @template param_measures
#'
#' @section S3 Methods:
#' * `as.data.table(rr, reassemble_learners = TRUE, convert_predictions = TRUE, predict_sets = "test")`\cr
#'   [ResampleResult] -> [data.table::data.table()]\cr
#'   Returns a tabular view of the internal data.
#' * `c(...)`\cr
#'   ([ResampleResult], ...) -> [BenchmarkResult]\cr
#'   Combines multiple objects convertible to [BenchmarkResult] into a new [BenchmarkResult].
#'
#' @export
#' @examples
#' task = tsk("penguins")
#' learner = lrn("classif.rpart")
#' resampling = rsmp("cv", folds = 3)
#' rr = resample(task, learner, resampling)
#' print(rr)
#'
#' rr$aggregate(msr("classif.acc"))
#' rr$prediction()
#' rr$prediction()$confusion
#' rr$warnings
#' rr$errors
ResampleResult = R6Class("ResampleResult",
  public = list(
    #' @field data (`ResultData`)\cr
    #' Internal data storage object of type `ResultData`.
    #' We discourage users to directly work with this field.
    #' Use `as.table.table(ResampleResult)` instead.
    data = NULL,

    #' @field view (`character(1)`)\cr
    #' Subset of uhashes in the [ResultData] object to operate on.
    #' This field is for internal optimizations, i.e. to avoid unnecessary cloning.
    view = NULL,

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #' An alternative construction method is provided by [as_resample_result()].
    #'
    #' @param data ([ResultData] | [data.table()])\cr
    #'   An object of type [ResultData], either extracted from another [ResampleResult], another
    #'   [BenchmarkResult], or manually constructed with [as_result_data()].
    #' @param view (`character()`)\cr
    #'   Single `uhash` of the [ResultData] to operate on.
    #'   Used internally for optimizations.
    initialize = function(data = ResultData$new(), view = NULL) {
      self$data = assert_class(data, "ResultData")
      self$view = assert_string(view, null.ok = TRUE)
    },

    #' @description
    #' Helper for print outputs.
    format = function() {
      sprintf("<%s>", class(self)[1L])
    },

    #' @description
    #' Printer.
    #' @param ... (ignored).
    print = function() {
      catf("%s of %i iterations", format(self), self$data$iterations(self$view))
      catf(str_indent("* Task:", self$task$id))
      catf(str_indent("* Learner:", self$learner$id))

      warnings = self$warnings
      catf(str_indent("* Warnings:", sprintf("%i in %i iterations", nrow(warnings), uniqueN(warnings, by = "iteration"))))

      errors = self$errors
      catf(str_indent("* Errors:", sprintf("%i in %i iterations", nrow(errors), uniqueN(errors, by = "iteration"))))
    },

    #' @description
    #' Opens the corresponding help page referenced by field `$man`.
    help = function() {
      open_help("mlr3::ResampleResult")
    },

    #' @description
    #' Combined [Prediction] of all individual resampling iterations, and all provided predict sets.
    #' Note that performance measures do not operate on this object,
    #' but instead on each prediction object separately and then combine the performance scores
    #' with the aggregate function of the respective [Measure].
    #'
    #' @param predict_sets (`character()`)\cr
    #' @return [Prediction].
    #'   Subset of `{"train", "test"}`.
    prediction = function(predict_sets = "test") {
      self$data$prediction(self$view, predict_sets)
    },

    #' @description
    #' List of prediction objects, sorted by resampling iteration.
    #' If multiple sets are given, these are combined to a single one for each iteration.
    #'
    #' @param predict_sets (`character()`)\cr
    #'   Subset of `{"train", "test"}`.
    #' @return List of [Prediction] objects, one per element in `predict_sets`.
    predictions = function(predict_sets = "test") {
      self$data$predictions(self$view, predict_sets)
    },

    #' @description
    #' Returns a table with one row for each resampling iteration, including all involved objects:
    #' [Task], [Learner], [Resampling], iteration number (`integer(1)`), and [Prediction].
    #' Additionally, a column with the individual (per resampling iteration) performance is added
    #' for each [Measure] in `measures`,
    #' named with the id of the respective measure id.
    #' If `measures` is `NULL`, `measures` defaults to the return value of [default_measures()].
    #'
    #' @param ids (`logical(1)`)\cr
    #'   If `ids` is `TRUE`, extra columns with the ids of objects (`"task_id"`, `"learner_id"`, `"resampling_id"`)
    #'   are added to the returned table.
    #'   These allow to subset more conveniently.
    #'
    #' @param conditions (`logical(1)`)\cr
    #'   Adds condition messages (`"warnings"`, `"errors"`) as extra
    #'   list columns of character vectors to the returned table
    #'
    #' @param predict_sets (`character()`)\cr
    #'   Vector of predict sets (`{"train", "test"}`) to construct the [Prediction] objects from.
    #'   Default is `"test"`.
    #'
    #' @return [data.table::data.table()].
    score = function(measures = NULL, ids = TRUE, conditions = FALSE, predict_sets = "test") {
      measures = as_measures(measures, task_type = self$data$task_type)
      assert_measures(measures, task = self$task, learner = self$learner)
      assert_flag(ids)
      assert_flag(conditions)
      assert_subset(predict_sets, mlr_reflections$predict_sets)

      tab = score_measures(self, measures, view = self$view)

      if (ids) {
        set(tab, j = "task_id", value = ids(tab[["task"]]))
        set(tab, j = "learner_id", value = ids(tab[["learner"]]))
        set(tab, j = "resampling_id", value = ids(tab[["resampling"]]))
        setcolorder(tab, c("task", "task_id", "learner", "learner_id", "resampling", "resampling_id",
            "iteration", "prediction"))
      }

      if (conditions) {
        set(tab, j = "warnings", value = map(tab$learner, "warnings"))
        set(tab, j = "errors", value = map(tab$learner, "errors"))
      }

      set(tab, j = "prediction", value = as_predictions(tab$prediction, predict_sets))
      cns = c("task", "task_id", "learner", "learner_id", "resampling", "resampling_id", "iteration",
        "prediction", "warnings", "errors", ids(measures))
      cns = intersect(cns, names(tab))
      tab[, cns, with = FALSE]
    },

    #' @description
    #' Calculates and aggregates performance values for all provided measures, according to the
    #' respective aggregation function in [Measure].
    #' If `measures` is `NULL`, `measures` defaults to the return value of [default_measures()].
    #'
    #' @return Named `numeric()`.
    aggregate = function(measures = NULL) {
      measures = as_measures(measures, task_type = self$data$task_type)
      assert_measures(measures, task = self$task, learner = self$learner)
      set_names(map_dbl(measures, function(m) m$aggregate(self)), ids(measures))
    },

    #' @description
    #' Subsets the [ResampleResult], reducing it to only keep the iterations specified in `iters`.
    #'
    #' @param iters (`integer()`)\cr
    #'   Resampling iterations to keep.
    #'
    #' @return
    #' Returns the object itself, but modified **by reference**.
    #' You need to explicitly `$clone()` the object beforehand if you want to keeps
    #' the object in its previous state.
    filter = function(iters) {
      iters = assert_integerish(iters, lower = 1L, upper = self$resampling$iters,
        any.missing = FALSE, unique = TRUE, coerce = TRUE)

      self$data = self$data$clone(deep = TRUE)
      fact = self$data$data$fact
      if (!is.null(self$view)) {
        fact = fact[list(self$view), on = "uhash", nomatch = NULL]
      }

      self$data$data$fact = fact[list(iters), on = "iteration", nomatch = NULL]

      invisible(self)
    },

    #' @description
    #' Repeats the resampling with retrainable models. The models are updated
    #' with the additional budget, the training continues on the training sets
    #' and the performance is again evaluated on the test sets.
    #'
    #' @param param_vals (`list()`)\cr
    #'   List of hyperparameter values.
    #' @param store_models (`logical(1)`)\cr
    #'   Keep the fitted model after the test set has been predicted?
    #'   Set to `TRUE` if you want to further analyse the models or want to
    #'   extract information like variable importance.
    #' @return
    #' Returns the object itself, but modified **by reference**.
    #' You need to explicitly `$clone()` the object beforehand if you want to keeps
    #' the object in its previous state.
    #'
    #' @note
    #' The fitted models are discarded after the predictions have been computed in
    #' order to reduce memory consumption. If you need access to the models for
    #' later analysis, set `store_models` to `TRUE`.
    retrain = function(param_vals, store_models = FALSE) {
      assert_flag(store_models)
      
      # Set new parameter set in learners with stored model
      learners = map(self$learners, function(l) {
        l$param_set$values = insert_named(l$param_set$values, param_vals)
        l
      })

      instance = self$resampling
      task = self$task
      n = instance$iters
      pb = get_progressor(n)

      lg$debug("Retrain via future with %i iterations", n)

      res = future.apply::future_mapply(workhorse, iteration = seq_len(n), learner = learners,
        MoreArgs = list(task = task, resampling = instance, store_models = store_models,
          lgr_threshold = lg$threshold, pb = pb, mode = ifelse(self$is_retrainable(param_vals), "retrain", "train")),
        future.globals = FALSE, future.scheduling = structure(TRUE, ordering = "random"),
        future.packages = "mlr3", future.seed = TRUE
      )

      data = data.table(
        task = list(task),
        learner = learners,
        learner_state = res["learner_state", ],
        resampling = list(instance),
        iteration = seq_len(n),
        prediction = res["prediction", ],
        uhash = UUIDgenerate()
      )

      self$data = ResultData$new(data)
      invisible(self)
    },

    #' @description
    #' Returns `TRUE` if model is retrainable with parameter values in `param_vals`.
    #' 
    #' @param param_vals (`list()`)\cr
    #'   List of hyperparameter values.
    #'
    #' @return `logical(1)`
    is_retrainable = function(param_vals) {
      all(map_lgl(self$learners, function(l) l$is_retrainable(param_vals)))
    }
  ),

  active = list(
    #' @field task_type (`character(1)`)\cr
    #' Task type of objects in the `ResampleResult`, e.g. `"classif"` or `"regr"`.
    #' This is `NA` for empty [ResampleResult]s.
    task_type = function(rhs) {
      assert_ro_binding(rhs)
      self$data$task_type
    },

    #' @field uhash (`character(1)`)\cr
    #' Unique hash for this object.
    uhash = function(rhs) {
      assert_ro_binding(rhs)
      uhash = self$data$uhashes(self$view)
      if (length(uhash) == 0L) NA_character_ else uhash
    },

    #' @field task ([Task])\cr
    #' The task [resample()] operated on.
    task = function(rhs) {
      assert_ro_binding(rhs)
      tab = self$data$tasks(self$view)
      if (nrow(tab) == 0L)
        return(NULL)
      tab$task[[1L]]
    },

    #' @field learner ([Learner])\cr
    #' Learner prototype [resample()] operated on.
    #' For a list of **trained** learners, see methods `$learners()`.
    learner = function(rhs) {
      assert_ro_binding(rhs)
      tab = self$data$learners(self$view, states = FALSE)
      if (nrow(tab) == 0L)
        return(NULL)
      tab$learner[[1L]]
    },

    #' @field resampling ([Resampling])\cr
    #' Instantiated [Resampling] object which stores the splits into training and test.
    resampling = function(rhs) {
      assert_ro_binding(rhs)
      tab = self$data$resamplings(self$view)
      if (nrow(tab) == 0L)
        return(NULL)
      tab$resampling[[1L]]
    },

    #' @field learners (list of [Learner])\cr
    #' List of trained learners, sorted by resampling iteration.
    learners = function(rhs) {
      assert_ro_binding(rhs)
      self$data$learners(self$view)$learner
    },

    #' @field warnings ([data.table::data.table()])\cr
    #' A table with all warning messages.
    #' Column names are `"iteration"` and `"msg"`.
    #' Note that there can be multiple rows per resampling iteration if multiple warnings have been recorded.
    warnings = function(rhs) {
      assert_ro_binding(rhs)
      self$data$logs(self$view, "warning")
    },

    #' @field errors ([data.table::data.table()])\cr
    #' A table with all error messages.
    #' Column names are `"iteration"` and `"msg"`.
    #' Note that there can be multiple rows per resampling iteration if multiple errors have been recorded.
    errors = function(rhs) {
      assert_ro_binding(rhs)
      self$data$logs(self$view, "error")
    }
  ),

  private = list(
    deep_clone = function(name, value) {
      if (name == "data") value$clone(deep = TRUE) else value
    }
  )
)

#' @export
as.data.table.ResampleResult = function(x, ..., predict_sets = "test") { # nolint
  tab = x$data$as_data_table(view = x$view, predict_sets = predict_sets)
  tab[, c("task", "learner", "resampling", "iteration", "prediction"), with = FALSE]
}

#' @export
c.ResampleResult = function(...) {
  do.call(c, lapply(list(...), as_benchmark_result))
}

#' @title Learner Class
#'
#' @include mlr_reflections.R
#'
#' @description
#' This is the abstract base class for learner objects like [LearnerClassif] and [LearnerRegr].
#'
#' Learners are build around the three following key parts:
#'
#' * Methods `$train()` and `$predict()` which call internal methods (either public method `$train_internal()`/`$predict_internal()` (soft deprecated)
#'   or private methods `$.train()`/`$.predict()`).
#' * A [paradox::ParamSet] which stores meta-information about available hyperparameters, and also stores hyperparameter settings.
#' * Meta-information about the requirements and capabilities of the learner.
#' * The fitted model stored in field `$model`, available after calling `$train()`.
#'
#' Predefined learners are stored in the [dictionary][mlr3misc::Dictionary] [mlr_learners],
#' e.g. [`classif.rpart`][mlr_learners_classif.rpart] or [`regr.rpart`][mlr_learners_regr.rpart].
#'
#' More classification and regression learners are implemented in the add-on package \CRANpkg{mlr3learners}.
#' Learners for survival analysis (or more general, for probabilistic regression) can be found in \CRANpkg{mlr3proba}.
#' The dictionary [mlr_learners] gets automatically populated with the new learners as soon as the respective packages are loaded.
#'
#' More (experimental) learners can be found on GitHub: \url{https://github.com/mlr3learners/}.
#' A guide on how to extend \CRANpkg{mlr3} with custom learners can be found in the [mlr3book](https://mlr3book.mlr-org.com).
#'
#' @template param_id
#' @template param_task_type
#' @template param_param_set
#' @template param_predict_types
#' @template param_feature_types
#' @template param_learner_properties
#' @template param_data_formats
#' @template param_packages
#' @template param_man
#'
#'
#' @section Optional Extractors:
#'
#' Specific learner implementations are free to implement additional getters to ease the access of certain parts
#' of the model in the inherited subclasses.
#'
#' For the following operations, extractors are standardized:
#'
#' * `importance(...)`: Returns the feature importance score as numeric vector.
#'   The higher the score, the more important the variable.
#'   The returned vector is named with feature names and sorted in decreasing order.
#'   Note that the model might omit features it has not used at all.
#'   The learner must be tagged with property `"importance"`.
#'   To filter variables using the importance scores, see package \CRANpkg{mlr3filters}.
#'
#' * `selected_features(...)`: Returns a subset of selected features as `character()`.
#'   The learner must be tagged with property `"selected_features"`.
#'
#' * `oob_error(...)`: Returns the out-of-bag error of the model as `numeric(1)`.
#'   The learner must be tagged with property `"oob_error"`.
#'
#'
#' @section Setting Hyperparameters:
#'
#' All information about hyperparameters is stored in the slot `param_set` which is a [paradox::ParamSet].
#' The printer gives an overview about the ids of available hyperparameters, their storage type, lower and upper bounds,
#' possible levels (for factors), default values and assigned values.
#' To set hyperparameters, assign a named list to the subslot `values`:
#' ```
#' lrn = lrn("classif.rpart")
#' lrn$param_set$values = list(minsplit = 3, cp = 0.01)
#' ```
#' Note that this operation replaces all previously set hyperparameter values.
#' If you only intend to change one specific hyperparameter value and leave the others as-is, you can use the helper function [mlr3misc::insert_named()]:
#' ```
#' lrn$param_set$values = mlr3misc::insert_named(lrn$param_set$values, list(cp = 0.001))
#' ```
#' If the learner has additional hyperparameters which are not encoded in the [ParamSet][paradox::ParamSet], you can easily extend the learner.
#' Here, we add a factor hyperparameter with id `"foo"` and possible levels `"a"` and `"b"`:
#' ```
#' lrn$param_set$add(paradox::ParamFct$new("foo", levels = c("a", "b")))
#' ```
#'
#' @family Learner
#' @export
Learner = R6Class("Learner",
  public = list(
    #' @template field_id
    id = NULL,

    #' @field state (`NULL` | named `list()`)\cr
    #' Current (internal) state of the learner.
    #' Contains all information gathered during `train()` and `predict()`.
    #' It is not recommended to access elements from `state` directly.
    #' This is an internal data structure which may change in the future.
    state = NULL,

    #' @template field_task_type
    task_type = NULL,

    #' @field predict_types (`character()`)\cr
    #' Stores the possible predict types the learner is capable of.
    #' A complete list of candidate predict types, grouped by task type, is stored in [`mlr_reflections$learner_predict_types`][mlr_reflections].
    predict_types = NULL,

    #' @field feature_types (`character()`)\cr
    #' Stores the feature types the learner can handle, e.g. `"logical"`, `"numeric"`, or `"factor"`.
    #' A complete list of candidate feature types, grouped by task type, is stored in [`mlr_reflections$task_feature_types`][mlr_reflections].
    feature_types = NULL,

    #' @field properties (`character()`)\cr
    #' Stores a set of properties/capabilities the learner has.
    #' A complete list of candidate properties, grouped by task type, is stored in [`mlr_reflections$learner_properties`][mlr_reflections].
    properties = NULL,


    #' @field data_formats (`character()`)\cr
    #' Supported data format, e.g. `"data.table"` or `"Matrix"`.
    data_formats = NULL,

    #' @template field_packages
    packages = NULL,

    #' @template field_predict_sets
    predict_sets = "test",

    #' @field fallback ([Learner])\cr
    #' Learner which is fitted to impute predictions in case that either the model fitting or the prediction of the top learner is not successful.
    #' Requires you to enable encapsulation, otherwise errors are not caught and the execution is terminated before the fallback learner kicks in.
    fallback = NULL,

    #' @template field_man
    man = NULL,

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' Note that this object is typically constructed via a derived classes, e.g. [LearnerClassif] or [LearnerRegr].
    initialize = function(id, task_type, param_set = ParamSet$new(), predict_types = character(), feature_types = character(),
      properties = character(), data_formats = "data.table", packages = character(), man = NA_character_) {

      self$id = assert_string(id, min.chars = 1L)
      self$task_type = assert_choice(task_type, mlr_reflections$task_types$type)
      private$.param_set = assert_param_set(param_set)
      private$.encapsulate = c(train = "none", predict = "none")
      self$feature_types = assert_subset(feature_types, mlr_reflections$task_feature_types)
      self$predict_types = assert_subset(predict_types, names(mlr_reflections$learner_predict_types[[task_type]]), empty.ok = FALSE)
      private$.predict_type = predict_types[1L]
      self$properties = sort(assert_subset(properties, mlr_reflections$learner_properties[[task_type]]))
      self$data_formats = assert_subset(data_formats, mlr_reflections$data_formats)
      self$packages = assert_set(packages)
      self$man = assert_string(man, na.ok = TRUE)

      check_packages_installed(packages, msg = sprintf("Package '%%s' required but not installed for Learner '%s'", id))
    },

    #' @description
    #' Helper for print outputs.
    format = function() {
      sprintf("<%s:%s>", class(self)[1L], self$id)
    },

    #' @description
    #' Printer.
    #' @param ... (ignored).
    print = function() {
      catf(format(self))
      catf(str_indent("* Model:", if (is.null(self$model)) "-" else class(self$model)[1L]))
      catf(str_indent("* Parameters:", as_short_string(self$param_set$values, 1000L)))
      catf(str_indent("* Packages:", self$packages))
      catf(str_indent("* Predict Type:", self$predict_type))
      catf(str_indent("* Feature types:", self$feature_types))
      catf(str_indent("* Properties:", self$properties))
      w = self$warnings
      e = self$errors
      if (length(w)) {
        catf(str_indent("* Warnings:", w))
      }
      if (length(e)) {
        catf(str_indent("* Errors:", e))
      }
    },

    #' @description
    #' Opens the corresponding help page referenced by field `$man`.
    help = function() {
      open_help(self$man)
    },

    #' @description
    #' Train the learner on a set of observations of the provided `task`.
    #' Mutates the learner by reference, i.e. stores the model alongside other information in field `$state`.
    #'
    #' @param task ([Task]).
    #'
    #' @param row_ids (`integer()`)\cr
    #'   Vector of training indices.
    #'
    #' @return
    #' Returns the object itself, but modified **by reference**.
    #' You need to explicitly `$clone()` the object beforehand if you want to keeps
    #' the object in its previous state.
    train = function(task, row_ids = NULL) {
      task = assert_task(as_task(task))
      assert_learnable(task, self)
      row_ids = assert_row_ids(row_ids, null.ok = TRUE)

      learner_train(self, task, row_ids)

      # store the task w/o the data
      self$state$train_task = task_rm_data(task$clone(deep = TRUE))

      invisible(self)
    },

    #' @description
    #' Uses the information stored during `$train()` in `$state` to create a new [Prediction]
    #' for a set of observations of the provided `task`.
    #'
    #' @param task ([Task]).
    #'
    #' @param row_ids (`integer()`)\cr
    #'   Vector of test indices.
    #'
    #' @return [Prediction].
    predict = function(task, row_ids = NULL) {
      task = assert_task(as_task(task))
      assert_learnable(task, self)
      row_ids = assert_row_ids(row_ids, null.ok = TRUE)

      if (is.null(self$state$model) && is.null(self$state$fallback_state$model)) {
        stopf("Cannot predict, Learner '%s' has not been trained yet", self$id)
      }

      pred = learner_predict(self, task, row_ids)
      create_prediction(pred, task_type = self$task_type)
    },

    #' @description
    #' Uses the model fitted during `$train()` to create a new [Prediction] based on the new data in `newdata`.
    #' Object `task` is the task used during `$train()` and required for conversion of `newdata`.
    #' If the learner's `$train()` method has been called, there is a (size reduced) version of the training task stored in the learner.
    #' If the learner has been fitted via [resample()] or [benchmark()], you need to pass the corresponding task stored
    #' in the [ResampleResult] or [BenchmarkResult], respectively.
    #'
    #' @param newdata (`data.frame()`)\cr
    #'   New data to predict on.
    #'   Row ids are automatically created via auto-incrementing.
    #'
    #' @param task ([Task]).
    #'
    #' @return [Prediction].
    predict_newdata = function(newdata, task = NULL) {
      newdata = as.data.table(assert_data_frame(newdata, min.rows = 1L))

      if (is.null(task)) {
        if (is.null(self$state$train_task))
          stopf("No task stored, and no task provided")
        task = self$state$train_task$clone()
      } else {
        task = assert_task(as_task(task, clone = TRUE))
        assert_learnable(task, self)
        task = task_rm_data(task)
      }

      # the following columns are automatically set to NA if missing
      impute = unlist(task$col_roles[c("target", "name", "order", "stratum", "group")])
      impute = setdiff(impute, c(task$feature_names, colnames(newdata)))
      newdata = insert_named(newdata, named_list(nn = impute, init = NA))

      self$predict(task$rbind(newdata))
    },

    #' @description
    #' Reset the learner, i.e. un-train by resetting the `state`.
    #'
    #' @return
    #' Returns the object itself, but modified **by reference**.
    #' You need to explicitly `$clone()` the object beforehand if you want to keeps
    #' the object in its previous state.
    reset = function() {
      self$state = NULL
      invisible(self)
    }
  ),

  active = list(
    #' @field model (`any`)\cr
    #' The fitted model. Only available after `$train()` has been called.
    model = function(rhs) {
      assert_ro_binding(rhs)
      self$state$model
    },


    #' @field timings (named `numeric(2)`)\cr
    #' Elapsed time in seconds for the steps `"train"` and `"predict"`.
    #' Measured via [mlr3misc::encapsulate()].
    timings = function(rhs) {
      assert_ro_binding(rhs)
      set_names(c(self$state$train_time %??% NA_real_, self$state$predict_time %??% NA_real_), c("train", "predict"))
    },

    #' @field log ([data.table::data.table()])\cr
    #' Returns the output (including warning and errors) as table with columns
    #'
    #' * `"stage"` ("train" or "predict"),
    #' * `"class"` ("output", "warning", or "error"), and
    #' * `"msg"` (`character()`).
    log = function(rhs) {
      assert_ro_binding(rhs)
      self$state$log
    },

    #' @field warnings (`character()`)\cr
    #' Logged warnings as vector.
    warnings = function(rhs) {
      assert_ro_binding(rhs)
      get_log_condition(self$state, "warning")
    },

    #' @field errors (`character()`)\cr
    #' Logged errors as vector.
    errors = function(rhs) {
      assert_ro_binding(rhs)
      get_log_condition(self$state, "error")
    },


    #' @template field_hash
    hash = function(rhs) {
      assert_ro_binding(rhs)
      hash(class(self), self$id, self$param_set$values, private$.predict_type, self$fallback$hash)
    },

    #' @field predict_type (`character(1)`)\cr
    #' Stores the currently active predict type, e.g. `"response"`.
    #' Must be an element of `$predict_types`.
    predict_type = function(rhs) {
      if (missing(rhs)) {
        return(private$.predict_type)
      }
      if (rhs %nin% self$predict_types) {
        stopf("Learner '%s' does not support predict type '%s'", self$id, rhs)
      }
      private$.predict_type = rhs
    },

    #' @template field_param_set
    param_set = function(rhs) {
      if (!missing(rhs) && !identical(rhs, private$.param_set)) {
        stop("param_set is read-only.")
      }
      private$.param_set
    },

    #' @field encapsulate (named `character()`)\cr
    #' Controls how to execute the code in internal train and predict methods.
    #' Must be a named character vector with names `"train"` and `"predict"`.
    #' Possible values are `"none"`, `"evaluate"` (requires package \CRANpkg{evaluate}) and `"callr"` (requires package \CRANpkg{callr}).
    #' See [mlr3misc::encapsulate()] for more details.
    encapsulate = function(rhs) {
      if (missing(rhs)) {
        return(private$.encapsulate)
      }
      assert_character(rhs)
      assert_names(names(rhs), subset.of = c("train", "predict"))
      private$.encapsulate = insert_named(c(train = "none", predict = "none"), rhs)
    }
  ),

  private = list(
    .encapsulate = NULL,
    .predict_type = NULL,
    .param_set = NULL,

    deep_clone = function(name, value) {
      switch(name,
        .param_set = value$clone(deep = TRUE),
        fallback = if (is.null(value)) NULL else value$clone(deep = TRUE),
        state = { value$log = copy(value$log); value },
        value
      )
    }
  )
)


#' @export
rd_info.Learner = function(obj) {
  c("",
    sprintf("* Task type: %s", rd_format_string(obj$task_type)),
    sprintf("* Predict Types: %s", rd_format_string(obj$predict_types)),
    sprintf("* Feature Types: %s", rd_format_string(obj$feature_types)),
    sprintf("* Required Packages: %s", rd_format_packages(obj$packages))
  )
}

get_log_condition = function(state, condition) {
  if (is.null(state$log)) {
    character()
  } else {
    msg = NULL
    state$log[condition, msg, on = "class", nomatch = NULL]
  }
}

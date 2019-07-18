#' @title Learner Class
#'
#' @usage NULL
#' @format [R6::R6Class] object.
#' @include mlr_reflections.R
#'
#' @description
#' This is the abstract base class for learner objects like [LearnerClassif] and [LearnerRegr].
#'
#' Learners consist of the following parts:
#'
#' - Methods `train()` and `predict()` which call `train_internal()` and `predict_internal()`.
#' - The fitted model, after calling `train()`.
#' - A [paradox::ParamSet] which stores meta-information about available hyperparameters, and also stores hyperparameter settings.
#' - Meta-information about the requirements and capabilities of the learner.
#'
#' Predefined learners are stored in the [Dictionary] [mlr_learners],
#' e.g. [`classif.rpart`][mlr_learners_classif.rpart] or [`regr.rpart`][mlr_learners_regr.rpart].
#' A guide on how to extend \CRANpkg{mlr3} with custom learners can be found in the [mlr3book](https://mlr3book.mlr-org.com).
#'
#'
#' @section Construction:
#' Note: This object is typically constructed via a derived classes, e.g. [LearnerClassif] or [LearnerRegr].
#'
#' ```
#' l = Learner$new(id, task_type, param_set = ParamSet$new(), param_vals = list(), predict_types = character(),
#'      feature_types = character(), properties = character(), packages = character())
#' ```
#'
#' * `id` :: `character(1)`\cr
#'   Identifier for the learner.
#'
#' * `task_type` :: `character(1)`\cr
#'   Type of the task the learner can operator on. E.g., `"classif"` or `"regr"`.
#'
#' * `param_set` :: [paradox::ParamSet]\cr
#'   Set of hyperparameters.
#'
#' * `param_vals` :: named `list()`\cr
#'   List of hyperparameter settings.
#'
#' * `predict_types` :: `character()`\cr
#'   Supported predict types. Must be a subset of [`mlr_reflections$learner_predict_types`][mlr_reflections].
#'
#' * `feature_types` :: `character()`\cr
#'   Feature types the learner operates on. Must be a subset of [`mlr_reflections$task_feature_types`][mlr_reflections].
#'
#' * `properties` :: `character()`\cr
#'   Set of properties of the learner. Must be a subset of [`mlr_reflections$learner_properties`][mlr_reflections].
#'
#' * `data_formats` :: `character()`\cr
#'   Vector of supported data formats which can be processed during `$train()` and `$predict()`.
#'   Defaults to `"data.table"`.
#'
#' * `packages` :: `character()`\cr
#'   Set of required packages.
#'   Note that these packages will be loaded via [requireNamespace()], and are not attached.
#'
#' @section Fields:
#' * `id` :: `character(1)`\cr
#'   Identifier of the learner.
#'
#' * `task_type` :: `character(1)`\cr
#'   Stores the type of class this learner can operate on, e.g. `"classif"` or `"regr"`.
#'   A complete list of task types is stored in [`mlr_reflections$task_types`][mlr_reflections].
#'
#' * `param_set` :: [paradox::ParamSet]\cr
#'   Description of available hyperparameters and hyperparameter settings.
#'
#' * `predict_types` :: `character()`\cr
#'   Stores the possible predict types the learner is capable of.
#'   A complete list of candidate predict types, grouped by task type, is stored in [`mlr_reflections$learner_predict_types`][mlr_reflections].
#'
#' * `predict_type` :: `character(1)`\cr
#'   Stores the currently selected predict type. Must be an element of `l$predict_types`.
#'
#' * `feature_types` :: `character()`\cr
#'   Stores the feature types the learner can handle, e.g. `"logical"`, `"numeric"`, or `"factor"`.
#'   A complete list of candidate feature types, grouped by task type, is stored in [`mlr_reflections$task_feature_types`][mlr_reflections].
#'
#' * `properties` :: `character()`\cr
#'   Stores a set of properties/capabilities the learner has.
#'   A complete list of candidate properties, grouped by task type, is stored in [`mlr_reflections$learner_properties`][mlr_reflections].
#'
#' * `packages` :: `character()`\cr
#'   Stores the names of required packages.
#'
#' * `encapsulate` (named `character()`)\cr
#'   How to call the code in `train_internal()` and `predict_internal()`.
#'   Must be a named character vector with names `"train"` and `"predict"`.
#'     - If set to `"none"` (default), the code is executed in the running session without error handling.
#'       Output is not stored, just send to the console.
#'     - If set to `"evaluate"`, the exceptions are caught using [evaluate::evaluate()].
#'       All output can be accessed via the learner field `$log`.
#'       \CRANpkg{evaluate} does not start a separate session, and thus cannot guard you against segfaults.
#'     - If set to `"callr"`, the code is executed in an independent R session using the \CRANpkg{callr} package.
#'       All output can be accessed via the learner field `$log`.
#'       This guards your session from segfaults, at the cost of some computational overhead.
#' * `fallback` ([Learner])\cr
#'   Learner which is fitted to impute predictions in case that either the model fitting or the prediction of the top learner is not successful.
#'   Requires you to enable encapsulation, otherwise errors are not caught and the execution is terminated before the fallback learner kicks in.
#'
#' * `hash` :: `character(1)`\cr
#'   Hash (unique identifier) for this object.
#'
#' * `model` :: `any`\cr
#'   The fitted model. Only available after `$train()` has been called.
#'
#' * `timings` :: `numeric(2)`\cr
#'   Elapsed time in seconds for the steps `"train"` and `"predict"`.
#'
#' * `log` :: [data.table::data.table()]\cr
#'   Returns the output (including warning and errors) as table with columns
#'   `"stage"` (train or predict), `"class"` (output, warning, error) and
#'   `"msg"` (`character()`).
#'
#' * `warnings` :: `character()`\cr
#'   Returns the logged warnings as vector.
#'
#' * `errors` :: `character()`\cr
#'   Returns the logged errors as vector.
#'
#' @section Methods:
#' * `train(task, row_ids = NULL, ctrl = list())`\cr
#'   ([Task], `integer()` | `character()`, [mlr_control()]) -> `self`\cr
#'   Train the learner on the row ids of the provided [Task].
#'   Mutates the learner by reference, i.e. stores the model alongside other objects in field `$data`.
#'
#' * `predict(task, row_ids = NULL, ctrl = list())`\cr
#'   ([Task], `integer()` | `character()`, [mlr_control()]) -> [Prediction]\cr
#'   Uses the data stored during `$train()` to create a new [Prediction] based on the provided `row_ids`
#'   of the `task`.
#'
#' * `predict_newdata(task, newdata, ctrl = list())`\cr
#'   ([Task], `data.frame()`, [mlr_control()]) -> [Prediction]\cr
#'   Uses the data stored during `$train()` to create a new [Prediction] based on the new data in `newdata`.
#'   Object `task` is the task used during `$train()` and required for conversions of `newdata`.
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
#'
#' * `selected_features(...)`: Returns a subset of selected features as `character()`.
#'   The learner must be tagged with property `"selected_features"`.
#'
#' * `oob_error(...)`: Returns the out-of-bag error of the model as `numeric(1)`.
#'   The learner must be tagged with property `"oob_error"`.
#'
#' @section Setting Hyperparameters:
#'
#' All information about hyperparameters is stored in the slot `param_set` which is a [paradox::ParamSet].
#' The printer gives an overview about the ids of available hyperparameters, their storage type, lower and upper bounds,
#' possible levels (for factors), default values and assigned values.
#' To set hyperparameters, assign a named list to the subslot `values`:
#' ```
#' lrn = mlr_learners$get("classif.rpart")
#' lrn$param_set$values = list(minsplit = 3, cp = 0.01)
#' ```
#' Note that this operation replaces all previously set hyperparameter values.
#' If you only intend to change one specific hyperparameter value and leave the others as-is, you can use the helper function [mlr3misc::insert_named()]:
#' ```
#' lrn$param_set$values = mlr3misc::insert_named(lrn$param_set$values, list(cp = 0.001))
#' ```
#' If the learner has additional hyperparameters which are not encoded in the [ParamSet][paradox::ParamSet], you can easily extend the learner.
#' Here, we add a hyperparameter with id `"foo"` possible levels `"a"` and `"b"`:
#' ```
#' lrn$param_set$add(paradox::ParamFct$new("foo", levels = c("a", "b")))
#' ```
#'
#' @family Learner
#' @export
Learner = R6Class("Learner",
  public = list(
    id = NULL,
    data = list(),
    task_type = NULL,
    predict_types = NULL,
    feature_types = NULL,
    properties = NULL,
    data_formats = NULL,
    packages = NULL,
    encapsulate = NULL,
    fallback = NULL,

    initialize = function(id, task_type, param_set = ParamSet$new(), param_vals = list(), predict_types = character(),
      feature_types = character(), properties = character(), data_formats = "data.table", packages = character()) {

      self$id = assert_string(id, min.chars = 1L)
      self$task_type = assert_choice(task_type, mlr_reflections$task_types)
      private$.param_set = assert_param_set(param_set)
      self$param_set$values = param_vals
      self$feature_types = assert_sorted_subset(feature_types, mlr_reflections$task_feature_types)
      self$predict_types = assert_sorted_subset(predict_types, names(mlr_reflections$learner_predict_types[[task_type]]), empty.ok = FALSE)
      private$.predict_type = predict_types[1L]
      self$packages = assert_set(packages)
      self$properties = sort(assert_subset(properties, mlr_reflections$learner_properties[[task_type]]))
      self$data_formats = assert_subset(data_formats, mlr_reflections$task_data_formats)
    },

    format = function() {
      sprintf("<%s:%s>", class(self)[1L], self$id)
    },

    print = function() {
      learner_print(self)
    },

    train = function(task, row_ids = NULL, ctrl = list()) {
      assert_task(task, task_type = self$task_type, feature_types = self$feature_types)
      if (!is.null(row_ids))
        row_ids = assert_row_ids(row_ids)
      ctrl = mlr_control(ctrl)
      invisible(learner_train(self, task, row_ids, ctrl))
    },

    predict = function(task, row_ids = NULL, ctrl = list()) {
      assert_task(task, task_type = self$task_type, feature_types = self$feature_types)
      if (!is.null(row_ids))
        row_ids = assert_row_ids(row_ids)
      ctrl = mlr_control(ctrl)
      if (is.null(self$data$predict_time)) {
        stopf("Learner not trained yet")
      }
      learner_predict(self, task, row_ids, ctrl)
    },

    predict_newdata = function(task, newdata, ctrl = list()) {
      assert_data_frame(newdata, min.rows = 1L)
      tn = task$target_names
      if (any(tn %nin% colnames(newdata))) {
        newdata[, tn] = NA
      }
      old_row_ids = task$row_ids
      task = task$clone(deep = TRUE)$rbind(newdata)
      row_ids = setdiff(task$row_ids, old_row_ids)
      self$predict(task, row_ids, ctrl = ctrl)
    }
  ),

  active = list(
    model = function() {
      self$data$model
    },

    timings = function() {
      set_names(c(self$data$train_time %??% NA_real_, self$data$predict_time %??% NA_real_), c("train", "predict"))
    },

    log = function() {
      tab = rbindlist(list(train = self$data$train_log, predict = self$data$predict_log), idcol = "stage", use.names = TRUE)
      if (nrow(tab) == 0L)
        tab = data.table(stage = character(), class = character(), msg = character())
      tab$stage = as_factor(tab$stage, levels = c("train", "predict"))
      tab$class = as_factor(tab$class, levels = mlr_reflections$log_classes)
      tab
    },

    warnings = function() {
      self$log[get("class") == "warning"]$msg
    },

    errors = function() {
      self$log[get("class") == "error"]$msg
    },

    hash = function() {
      hash(list(class(self), self$id, self$param_set$values, private$.predict_type))
    },

    predict_type = function(rhs) {
      if (missing(rhs)) {
        return(private$.predict_type)
      }
      assert_choice(rhs, names(mlr_reflections$learner_predict_types[[self$task_type]]))
      if (rhs %nin% self$predict_types) {
        stopf("Learner does not support predict type '%s'", rhs)
      }
      private$.predict_type = rhs
    },

    param_set = function(rhs) {
      if (!missing(rhs) && !identical(rhs, private$.param_set)) {
        stop("param_set is read-only.")
      }
      private$.param_set
    }
  ),

  private = list(
    .predict_type = NULL,
    .param_set = NULL
  )
)


learner_print = function(self) {
  catf(format(self))
  catf(str_indent("Model:", if (is.null(self$model)) "-" else class(self$model)[1L]))
  catf(str_indent("Parameters:", as_short_string(self$param_set$values, 1000L)))
  catf(str_indent("Packages:", self$packages))
  catf(str_indent("Predict Type:", self$predict_type))
  catf(str_indent("Feature types:", self$feature_types))
  catf(str_indent("Properties:", self$properties))
}

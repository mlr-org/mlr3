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
#' Predefined learners are stored in the [mlr3misc::Dictionary] [mlr_learners],
#' e.g. [`classif.rpart`][mlr_learners_classif.rpart] or [`regr.rpart`][mlr_learners_regr.rpart].
#' A guide on how to extend \CRANpkg{mlr3} with custom learners can be found in the [mlr3book](https://mlr3book.mlr-org.com).
#'
#'
#' @section Construction:
#' Note: This object is typically constructed via a derived classes, e.g. [LearnerClassif] or [LearnerRegr].
#'
#' ```
#' l = Learner$new(id, task_type, param_set = ParamSet$new(), predict_types = character(),
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
#' * `predict_types` :: `character()`\cr
#'   Supported predict types. Must be a subset of [`mlr_reflections$learner_predict_types`][mlr_reflections].
#'
#' * `predict_sets` :: `character()`\cr
#'   Sets to predict on during [resample()]/[benchmark()].
#'   Creates and stores a separate [Prediction] object for each set.
#'   The individual sets can be combined via getters in [ResampleResult]/[BenchmarkResult], or [Measure]s can be set to operate on subsets of the calculated prediction sets.
#'   Must be a non-empty subset of `("train", "test")`.
#'   Default is `"test"`.
#'
#' * `feature_types` :: `character()`\cr
#'   Feature types the learner operates on. Must be a subset of [`mlr_reflections$task_feature_types`][mlr_reflections].
#'
#' * `properties` :: `character()`\cr
#'   Set of properties of the learner. Must be a subset of [`mlr_reflections$learner_properties`][mlr_reflections].
#'   The following properties are currently standardized and understood by learners in \CRANpkg{mlr3}:
#'   * `"missings"`: The learner can handle missing values in the data.
#'   * `"weights"`: The learner supports observation weights.
#'   * `"importance"`: The learner supports extraction of importance scores, i.e. comes with a `importance()` extractor function (see section on optional extractors).
#'   * `"selected_features"`: The learner supports extraction of the set of selected features, i.e. comes with a `selected_features()` extractor function (see section on optional extractors).
#'   * `"oob_error"`: The learner supports extraction of estimated out of bag error, i.e. comes with a `oob_error()` extractor function (see section on optional extractors).
#'
#' * `data_formats` :: `character()`\cr
#'   Vector of supported data formats which can be processed during `$train()` and `$predict()`.
#'   Defaults to `"data.table"`.
#'
#' * `packages` :: `character()`\cr
#'   Set of required packages.
#'   Note that these packages will be loaded via [requireNamespace()], and are not attached.
#'
#' * `man` :: `character(1)`\cr
#'   String in the format `[pkg]::[topic]` pointing to a manual page for this object.
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
#' * `state` :: `NULL` | named `list()`\cr
#'   Current (internal) state of the learner.
#'   Contains all information learnt during `train()` and `predict()`.
#'   It is not recommended to access elements from `state` directly, this is an internal data structure which may change in the future.
#'
#' * `encapsulate` (named `character()`)\cr
#'   How to call the code in `train_internal()` and `predict_internal()`.
#'   Must be a named character vector with names `"train"` and `"predict"`.
#'   Possible values are `"none"`, `"evaluate"` and `"callr"`.
#'   See [mlr3misc::encapsulate()] for more details.
#'
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
#'   Measured via [mlr3misc::encapsulate()].
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
#' * `train(task, row_ids = NULL))`\cr
#'   ([Task], `integer()` | `character()`) -> `self`\cr
#'   Train the learner on the row ids of the provided [Task].
#'   Mutates the learner by reference, i.e. stores the model alongside other objects in field `$state`.
#'
#' * `predict(task, row_ids = NULL)`\cr
#'   ([Task], `integer()` | `character()`) -> [Prediction]\cr
#'   Uses the data stored during `$train()` in `$state` to create a new [Prediction] based on the provided `row_ids`
#'   of the `task`.
#'
#' * `predict_newdata(newdata, task = NULL)`\cr
#'   (`data.frame()`, [Task]) -> [Prediction]\cr
#'   Uses the model fitted during `$train()` in to create a new [Prediction] based on the new data in `newdata`.
#'   Object `task` is the task used during `$train()` and required for conversions of `newdata`.
#'   If the learner's `$train()` method has been called, there is a (size reduced) version of the training task stored in the learner.
#'   If the learner has been fitted via [resample()] or [benchmark()], you need to pass the corresponding task stored
#'   in the [ResampleResult] or [BenchmarkResult], respectively.
#'
#' * `reset()`\cr
#'   () -> `self`\cr
#'   Reset the learner, i.e. un-train by resetting the `state`.
#'
#' * `help()`\cr
#'   () -> `NULL`\cr
#'   Opens the corresponding help page referenced by `$man`.
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
#'   To filter variables using the importance scores, use package \CRANpkg{mlr3filters}.
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
#' lrn = lrn("classif.rpart")
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
    state = NULL,
    task_type = NULL,
    predict_types = NULL,
    feature_types = NULL,
    properties = NULL,
    data_formats = NULL,
    packages = NULL,
    predict_sets = "test",
    fallback = NULL,
    man = NULL,

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
    },

    help = function() {
      open_help(self$man)
    },

    format = function() {
      sprintf("<%s:%s>", class(self)[1L], self$id)
    },

    print = function() {
      learner_print(self)
    },

    train = function(task, row_ids = NULL) {
      task = assert_task(as_task(task))
      assert_learnable(task, self)

      if (!is.null(row_ids)) {
        row_ids = assert_row_ids(row_ids)
      }
      learner_train(self, task, row_ids)

      # store the task w/o the data
      self$state$train_task = task_rm_data(task$clone(deep = TRUE))

      invisible(self)
    },

    predict = function(task, row_ids = NULL) {
      task = assert_task(as_task(task))
      assert_learnable(task, self)

      if (!is.null(row_ids)) {
        row_ids = assert_row_ids(row_ids)
      }

      if (is.null(self$model) && is.null(self$state$fallback_state$model)) {
        stopf("Cannot predict, Learner '%s' has not been trained yet", self$id)
      }

      learner_predict(self, task, row_ids)
    },

    predict_newdata = function(newdata, task = NULL) {
      if (is.null(task)) {
        if (is.null(self$state$train_task))
          stopf("No task stored, and no task provided")
        task = self$state$train_task$clone()
      } else {
        task = assert_task(as_task(task, clone = TRUE))
        assert_learnable(task, self)
        task = task_rm_data(task)
      }

      newdata = assert_data_frame(newdata, min.rows = 1L)
      tn = task$target_names
      if (any(tn %nin% colnames(newdata))) {
        newdata[, tn] = NA
      }
      self$predict(task$rbind(newdata))
    },

    reset = function() {
      self$state = NULL
      invisible(self)
    }
  ),

  active = list(
    model = function() {
      self$state$model
    },

    timings = function() {
      set_names(c(self$state$train_time %??% NA_real_, self$state$predict_time %??% NA_real_), c("train", "predict"))
    },

    log = function() {
      self$state$log
    },

    warnings = function() {
      if (is.null(self$state$log)) {
        character()
      } else {
        self$log[class == "warning", msg]
      }
    },

    errors = function() {
      if (is.null(self$state$log)) {
        character()
      } else {
        self$log[class == "error", msg]
      }
    },

    hash = function() {
      hash(class(self), self$id, self$param_set$values, private$.predict_type, self$fallback$hash)
    },

    predict_type = function(rhs) {
      if (missing(rhs)) {
        return(private$.predict_type)
      }
      if (rhs %nin% self$predict_types) {
        stopf("Learner '%s' does not support predict type '%s'", self$id, rhs)
      }
      private$.predict_type = rhs
    },

    param_set = function(rhs) {
      if (!missing(rhs) && !identical(rhs, private$.param_set)) {
        stop("param_set is read-only.")
      }
      private$.param_set
    },

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
        state = { value$log = copy(value$log); value },
        value
      )
    }
  )
)


learner_print = function(self) {
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
}

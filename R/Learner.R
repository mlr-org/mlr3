#' @title Learner Class
#'
#' @include mlr_reflections.R
#' @include warn_deprecated.R
#'
#' @description
#' This is the abstract base class for learner objects like [LearnerClassif] and [LearnerRegr].
#'
#' Learners are build around the three following key parts:
#'
#' * Methods `$train()` and `$predict()` which call internal methods or private methods `$.train()`/`$.predict()`).
#' * A [paradox::ParamSet] which stores meta-information about available hyperparameters, and also stores hyperparameter settings.
#' * Meta-information about the requirements and capabilities of the learner.
#' * The fitted model stored in field `$model`, available after calling `$train()`.
#'
#' Predefined learners are stored in the [dictionary][mlr3misc::Dictionary] [mlr_learners],
#' e.g. [`classif.rpart`][mlr_learners_classif.rpart] or [`regr.rpart`][mlr_learners_regr.rpart].
#'
#' More classification and regression learners are implemented in the add-on package \CRANpkg{mlr3learners}.
#' Learners for survival analysis (or more general, for probabilistic regression) can be found in \CRANpkg{mlr3proba}.
#' Unsupervised cluster algorithms are implemented in \CRANpkg{mlr3cluster}.
#' The dictionary [mlr_learners] gets automatically populated with the new learners as soon as the respective packages are loaded.
#'
#' More (experimental) learners can be found in the GitHub repository: \url{https://github.com/mlr-org/mlr3extralearners}.
#' A guide on how to extend \CRANpkg{mlr3} with custom learners can be found in the [mlr3book](https://mlr3book.mlr-org.com).
#'
#' To combine the learner with preprocessing operations like factor encoding, \CRANpkg{mlr3pipelines} is recommended.
#' Hyperparameters stored in the `param_set` can be tuned with \CRANpkg{mlr3tuning}.
#'
#' @template param_id
#' @template param_task_type
#' @template param_param_set
#' @template param_predict_types
#' @template param_feature_types
#' @template param_learner_properties
#' @template param_data_formats
#' @template param_packages
#' @template param_label
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
#' * `internal_valid_scores`: Returns the internal validation score(s) of the model as a named `list()`.
#'   Only available for [`Learner`]s with the `"validation"` property.
#'   If the learner is not trained yet, this returns `NULL`.
#'
#' * `internal_tuned_values`: Returns the internally tuned hyperparameters of the model as a named `list()`.
#'   Only available for [`Learner`]s with the `"internal_tuning"` property.
#'   If the learner is not trained yet, this returns `NULL`.
#'
#' @section Weights:
#'
#' Many learners support observation weights, indicated by their property `"weights"`.
#' The weights are stored in the [Task] where the column role `weights_learner` needs to be assigned to a single numeric column.
#' If a task has weights and the learner supports them, they are used automatically.
#' If a task has weights but the learner does not support them, an error is thrown by default.
#' Both of these behaviors can be disabled by setting the `use_weights` field to `"ignore"`.
#' See the description of `use_weights` for more information.
#'
#' If the learner is set-up to use weights but the task does not have a designated weight column, samples are considered to have equal weight.
#' When weights are being used, they are passed down to the learner directly; the effect of weights depends on the specific learner.
#' Generally, weights do not need to sum up to 1.
#'
#' When implementing a Learner that uses weights, the `"weights"` property should be set.
#' The `$.train()`-method should then call the `$.get_weights()`-method to retrieve the weights from the task.
#' `$.get_weights()` will automatically discard weights when `use_weights` is set to `"ignore"`;
#'
#' @section Setting Hyperparameters:
#'
#' All information about hyperparameters is stored in the slot `param_set` which is a [paradox::ParamSet].
#' The printer gives an overview about the ids of available hyperparameters, their storage type, lower and upper bounds,
#' possible levels (for factors), default values and assigned values.
#' To set hyperparameters, call the `set_values()` method on the `param_set`:
#' ```
#' lrn = lrn("classif.rpart")
#' lrn$param_set$set_values(minsplit = 3, cp = 0.01)
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
#' @section Implementing Validation:
#' Some Learners, such as `XGBoost`, other boosting algorithms, or deep learning models (`mlr3torch`),
#' utilize validation data during the training to prevent overfitting or to log the validation performance.
#' It is possible to configure learners to be able to receive such an independent validation set during training.
#' To do so, one must:
#' * annotate the learner with the `"validation"` property
#' * implement the active binding `$internal_valid_scores` (see section *Optional Extractors*), as well as the
#'   private method `$.extract_internal_valid_scores()` which returns the (final) internal validation scores from the
#'   model of the [`Learner`] and returns them as a named `list()` of `numeric(1)`.
#'   If the model is not trained yet, this method should return `NULL`.
#' * Add the `validate` parameter, which can be either `NULL`, a ratio in $(0, 1)$, `"test"`, or `"predefined"`:
#'   * `NULL`: no validation
#'   * `ratio`: only proportion `1 - ratio` of the task is used for training and `ratio` is used for validation.
#'   * `"test"` means that the `"test"` task is used.
#'     **Warning**: This can lead to biased performance estimation.
#'     This option is only available if the learner is being trained via [resample()], [benchmark()] or functions that
#'     internally use them, e.g. `tune()` of \CRANpkg{mlr3tuning} or `batchmark()` of \CRANpkg{mlr3batchmark}.
#'     This is especially useful for hyperparameter tuning, where one might e.g. want to use the same validation data
#'     for early stopping and model evaluation.
#'   * `"predefined"` means that the task's (manually set) `$internal_valid_task` is used.
#'     See the [`Task`] documentation for more information.
#'
#' For an example how to do this, see [`LearnerClassifDebug`].
#' Note that in `.train()`, the `$internal_valid_task` will only be present if the `$validate` field of the `Learner`
#' is set to a non-`NULL` value.
#'
#' @section Implementing Internal Tuning:
#' Some learners such as `XGBoost` or `cv.glmnet` can internally tune hyperparameters.
#' XGBoost, for example, can tune the number of boosting rounds based on the validation performance.
#' CV Glmnet, on the other hand, can tune the regularization parameter based on an internal cross-validation.
#' Internal tuning *can* therefore rely on the internal validation data, but does not necessarily do so.
#'
#' In order to be able to combine this internal hyperparamer tuning with the standard hyperparameter optimization
#' implemented via \CRANpkg{mlr3tuning}, one most:
#' * annotate the learner with the `"internal_tuning"` property
#' * implement the active binding `$internal_tuned_values` (see section *Optional Extractors*) as well as the
#'   private method `$.extract_internal_tuned_values()` which extracts the internally tuned values from the [`Learner`]'s
#'   model and returns them as a named `list()`.
#'   If the model is not trained yet, this method should return `NULL`.
#' * Have at least one parameter tagged with `"internal_tuning"`, which requires to also provide a `in_tune_fn` and
#'   `disable_tune_fn`, and *should* also include a default `aggr`egation function.
#'
#' For an example how to do this, see [`LearnerClassifDebug`].
#'
#' @section Implementing Marshaling:
#' Some [`Learner`]s have models that cannot be serialized as they e.g. contain external pointers.
#' In order to still be able to save them, use them with parallelization or callr encapsulation it is necessary
#' to implement how they should be (un)-marshaled. See [`marshaling`] for how to do this.
#'
#' @template seealso_learner
#' @export
Learner = R6Class("Learner",
  public = list(
    #' @template field_id
    id = NULL,

    #' @template field_label
    label = NA_character_,

    #' @field state (`NULL` | named `list()`)\cr
    #' Current (internal) state of the learner.
    #' Contains all information gathered during `train()` and `predict()`.
    #' It is not recommended to access elements from `state` directly.
    #' This is an internal data structure which may change in the future.
    state = NULL,

    #' @template field_task_type
    task_type = NULL,

    #' @field feature_types (`character()`)\cr
    #' Stores the feature types the learner can handle, e.g. `"logical"`, `"numeric"`, or `"factor"`.
    #' A complete list of candidate feature types, grouped by task type, is stored in [`mlr_reflections$task_feature_types`][mlr_reflections].
    feature_types = NULL,

    #' @field properties (`character()`)\cr
    #' Stores a set of properties/capabilities the learner has.
    #' A complete list of candidate properties, grouped by task type, is stored in [`mlr_reflections$learner_properties`][mlr_reflections].
    properties = NULL,

    #' @template field_packages
    packages = NULL,

    #' @template field_predict_sets
    predict_sets = "test",

    #' @field parallel_predict (`logical(1)`)\cr
    #' If set to `TRUE`, use \CRANpkg{future} to calculate predictions in parallel (default: `FALSE`).
    #' The row ids of the `task` will be split into [future::nbrOfWorkers()] chunks,
    #' and predictions are evaluated according to the active [future::plan()].
    #' This currently only works for methods `Learner$predict()` and `Learner$predict_newdata()`,
    #' and has no effect during [resample()] or [benchmark()] where you have other means
    #' to parallelize.
    #'
    #' Note that the recorded time required for prediction reports the time required to predict
    #' is not properly defined and depends on the parallelization backend.
    parallel_predict = FALSE,

    #' @field timeout (named `numeric(2)`)\cr
    #' Timeout for the learner's train and predict steps, in seconds.
    #' This works differently for different encapsulation methods, see
    #' [mlr3misc::encapsulate()].
    #' Default is `c(train = Inf, predict = Inf)`.
    #' Also see the section on error handling the mlr3book:
    #' \url{https://mlr3book.mlr-org.com/chapters/chapter10/advanced_technical_aspects_of_mlr3.html#sec-error-handling}
    timeout = c(train = Inf, predict = Inf),

    #' @template field_man
    man = NULL,

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' Note that this object is typically constructed via a derived classes, e.g. [LearnerClassif] or [LearnerRegr].
    initialize = function(id, task_type, param_set = ps(), predict_types = character(), feature_types = character(),
      properties = character(), data_formats, packages = character(), label = NA_character_, man = NA_character_) {

      self$id = assert_string(id, min.chars = 1L)
      self$label = assert_string(label, na.ok = TRUE)
      self$task_type = assert_choice(task_type, mlr_reflections$task_types$type)
      self$feature_types = assert_ordered_set(feature_types, mlr_reflections$task_feature_types, .var.name = "feature_types")
      private$.predict_types = assert_ordered_set(predict_types, names(mlr_reflections$learner_predict_types[[task_type]]),
        empty.ok = FALSE, .var.name = "predict_types")
      private$.predict_type = predict_types[1L]
      self$properties = sort(assert_subset(properties, mlr_reflections$learner_properties[[task_type]]))
      if (!missing(data_formats)) warn_deprecated("Learner$initialize argument 'data_formats'")
      self$packages = union("mlr3", assert_character(packages, any.missing = FALSE, min.chars = 1L))
      self$man = assert_string(man, na.ok = TRUE)

      if ("weights" %in% self$properties) {
        self$use_weights = "use"
      } else {
        self$use_weights = "error"
      }
      private$.param_set = param_set

      check_packages_installed(packages, msg = sprintf("Package '%%s' required but not installed for Learner '%s'", id))
    },

    #' @description
    #' Helper for print outputs.
    #' @param ... (ignored).
    format = function(...) {
      sprintf("<%s:%s>", class(self)[1L], self$id)
    },

    #' @description
    #' Printer.
    #' @param ... (ignored).
    print = function(...) {
      msg_h =  if (is.null(self$label) || is.na(self$label)) "" else paste0(": ", self$label)
      model =  if (is.null(self$model)) "-" else if (is_marshaled_model(self$model)) "<marshaled>" else paste0(class(self$model)[1L])

      cat_cli({
        cli_h1("{.cls {class(self)[1L]}} ({self$id}){msg_h}")
        cli_li("Model: {model}")
        cli_li("Parameters: {as_short_string(self$param_set$values, 1000L)}")
      })

      if (exists("validate", self)) cat_cli(cli_li("Validate: {.cls {class(self$validate[1])}} {self$validate$id}"))
      cat_cli(cli_li("Packages: {.pkg {self$packages}}"))

      pred_typs = replace(self$predict_types, self$predict_types == self$predict_type, paste0("[", self$predict_type, "]"))
      encapsulation = self$encapsulation[[1]]
      fallback = if (encapsulation != 'none') class(self$fallback)[[1L]] else "-"

      cat_cli({
        cli_li("Predict Types: {pred_typs}")
        cli_li("Feature Types: {self$feature_types}")
        cli_li("Encapsulation: {encapsulation} (fallback: {fallback})")
        cli_li("Properties: {self$properties}")
        cli_li("Other settings: use_weights = '{self$use_weights}'")
      })

      w = self$warnings
      e = self$errors
      if (length(w)) {
        cat_cli(cli_alert_warning("Warnings: {w}"))
      }
      if (length(e)) {
        cat_cli(cli_alert_danger("Errors: {e}"))
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
    #'   Vector of training indices as subset of `task$row_ids`.
    #'   For a simple split into training and test set, see [partition()].
    #'
    #' @return
    #' Returns the object itself, but modified **by reference**.
    #' You need to explicitly `$clone()` the object beforehand if you want to keeps
    #' the object in its previous state.
    train = function(task, row_ids = NULL) {
      task = assert_task(as_task(task))
      assert_learnable(task, self)
      row_ids = assert_row_ids(row_ids, task = task, null.ok = TRUE)

      if (!is.null(self$hotstart_stack)) {
        # search for hotstart learner
        start_learner = get_private(self$hotstart_stack)$.start_learner(self, task$hash)
      }
      if (is.null(self$hotstart_stack) || is.null(start_learner)) {
        # no hotstart learners stored or no adaptable model found
        learner = self
        mode = "train"
      } else {
        self$state = start_learner$clone()$state
        learner = self
        mode = "hotstart"
      }

      train_row_ids = if (!is.null(row_ids)) row_ids else task$row_roles$use

      train_result = learner_train(learner, task, train_row_ids = train_row_ids, mode = mode)
      self$model = unmarshal_model(model = self$state$model, inplace = TRUE)

      # store data prototype
      proto = task$data(rows = integer())
      self$state$data_prototype = proto
      self$state$task_prototype = proto

      # store the task w/o the data
      self$state$train_task = task_rm_backend(task$clone(deep = TRUE))

      invisible(self)
    },
    #' @description
    #' Uses the fitted model stored in `$state` to generate predictions for a set of observations from the provided `task`.
    #' This method requires that the learner has been previously trained using `$train()`.
    #'
    #' @param task ([Task])\cr
    #'   The task containing the observations to predict on.
    #'   Must be compatible with the learner's task type and feature types.
    #'   Unlike `$predict_newdata()`, no type conversion is done.
    #'
    #' @param row_ids (`integer()`)\cr
    #'   Vector of row indices from `task$row_ids` to predict on.
    #'   If `NULL` (default), predictions are made for all rows in the task.
    #'   For a simple train-test split, see [partition()].
    #'
    #' @return [Prediction] object containing the predictions for the specified observations.
    predict = function(task, row_ids = NULL) {
      # improve error message for the common mistake of passing a data.frame here
      if (is.data.frame(task)) {
        stopf("To predict on data.frames, use the method `$predict_newdata()` instead of `$predict()`")
      }
      task = assert_task(as_task(task))
      assert_predictable(task, self)
      row_ids = assert_row_ids(row_ids, task = task, null.ok = TRUE)

      if (is.null(self$state$model) && is.null(self$state$fallback_state$model)) {
        stopf("Cannot predict, Learner '%s' has not been trained yet", self$id)
      }

      # we need to marshal for call-r prediction and parallel prediction, but afterwards we reset the model
      # to it original state
      model_was_marshaled = is_marshaled_model(self$model)
      on.exit({
        if (model_was_marshaled) {
          self$model = marshal_model(self$model, inplace = TRUE)
        } else {
          self$model = unmarshal_model(self$model, inplace = TRUE)
        }
      }, add = TRUE)

      # reset learner predict time; this is only cumulative for multiple predict sets,
      # not for multiple calls to predict / predict_newdata
      self$state$predict_time = 0

      # we only have to marshal here for the parallel prediction case, because learner_predict() handles the
      # marshaling for call-r encapsulation itself
      if (isTRUE(self$parallel_predict) && nbrOfWorkers() > 1L) {
        row_ids = row_ids %??% task$row_ids
        chunked = chunk_vector(row_ids, n_chunks = nbrOfWorkers(), shuffle = FALSE)
        self$model = marshal_model(self$model, inplace = TRUE)
        pdata = future.apply::future_lapply(chunked,
          learner_predict, learner = self, task = task,
          future.globals = FALSE, future.seed = TRUE)
        pdata = do.call(c, pdata)
      } else {
        pdata = learner_predict(self, task, row_ids)
      }

      if (is.null(pdata)) {
        return(NULL)
      } else {
        as_prediction(pdata)
      }
    },

    #' @description
    #' Uses the model fitted during `$train()` to create a new [Prediction] based on the new data in `newdata`.
    #' Object `task` is the task used during `$train()` and required for conversion of `newdata`.
    #' If the learner's `$train()` method has been called, there is a (size reduced) version
    #' of the training task stored in the learner.
    #' If the learner has been fitted via [resample()] or [benchmark()], you need to pass the corresponding task stored
    #' in the [ResampleResult] or [BenchmarkResult], respectively.
    #' Further, [`auto_convert`] is used for type-conversions to ensure compatability
    #' of features between `$train()` and `$predict()`.
    #'
    #' If the stored training task has a `weights_measure` column, *and* if `newdata` contains a column with the same name,
    #' that column must be numeric with no missing values and is used as measure weights column.
    #' Otherwise, no measure weights are used.
    #'
    #' @param newdata (any object supported by [as_data_backend()])\cr
    #'   New data to predict on.
    #'   All data formats convertible by [as_data_backend()] are supported, e.g.
    #'   `data.frame()` or [DataBackend].
    #'   If a [DataBackend] is provided as `newdata`, the row ids are preserved,
    #'   otherwise they are set to to the sequence `1:nrow(newdata)`.
    #'
    #' @param task ([Task]).
    #'
    #' @return [Prediction].
    predict_newdata = function(newdata, task = NULL) {
      if (is.null(task)) {
        if (is.null(self$state$train_task)) {
          stopf("No task stored, and no task provided")
        }
        task = self$state$train_task$clone()
      } else {
        task = assert_task(as_task(task, clone = TRUE))
        assert_learnable(task, self)
        task = task_rm_backend(task)
      }

      newdata = as_data_backend(newdata)
      assert_names(newdata$colnames, must.include = task$feature_names)

      # the following columns are automatically set to NA if missing
      # We do not impute weighs_measure, because we decidedly do not have weights_measure in this case.
      impute = unlist(task$col_roles[c("target", "name", "order", "stratum", "group")], use.names = FALSE)
      impute = setdiff(impute, newdata$colnames)
      tab1 = if (length(impute)) {
        # create list with correct NA types and cbind it to the backend
        ci = insert_named(task$col_info[list(impute), c("id", "type", "levels"), on = "id", with = FALSE], list(value = NA))
        na_cols = set_names(pmap(ci, function(..., nrow) rep(auto_convert(...), nrow), nrow = newdata$nrow), ci$id)
        invoke(data.table, .args = insert_named(na_cols, set_names(list(newdata$rownames), newdata$primary_key)))
      }

      # Perform type conversion where necessary
      keep_cols = intersect(newdata$colnames, task$col_info$id)
      ci = task$col_info[list(keep_cols), ][
        get("type") != col_info(newdata)[list(keep_cols), on = "id"]$type]
      tab2 = do.call(data.table, Map(auto_convert,
        value = as.list(newdata$data(rows = newdata$rownames, cols = ci$id)),
        id = ci$id, type = ci$type, levels = ci$levels))

      tab = cbind(tab1, tab2)
      if (ncol(tab)) {
        tab[[newdata$primary_key]] = newdata$rownames
        newdata = DataBackendCbind$new(newdata, DataBackendDataTable$new(tab, primary_key = newdata$primary_key))
      }

      prevci = task$col_info
      task$backend = newdata
      task$col_info = col_info(task$backend)
      task$col_info[, c("label", "fix_factor_levels")] = prevci[list(task$col_info$id), on = "id", c("label", "fix_factor_levels")]
      task$col_info$fix_factor_levels[is.na(task$col_info$fix_factor_levels)] = FALSE
      task$row_roles$use = task$backend$rownames
      task_col_roles = task$col_roles
      update_col_roles = FALSE
      if (any(task_col_roles$weights_measure %nin% newdata$colnames)) {
        update_col_roles = TRUE
        task_col_roles$weights_measure = character(0)
      }
      if (any(task_col_roles$weights_learner %nin% newdata$colnames)) {
        update_col_roles = TRUE
        task_col_roles$weights_learner = character(0)
      }
      if (update_col_roles) {
        task$col_roles = task_col_roles
      }

      self$predict(task)
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
    },

    #' @description
    #' Extracts the base learner from nested learner objects like
    #' `GraphLearner` in \CRANpkg{mlr3pipelines} or `AutoTuner` in
    #' \CRANpkg{mlr3tuning}.
    #' Returns the [Learner] itself for regular learners.
    #'
    #' @param recursive (`integer(1)`)\cr
    #'   Depth of recursion for multiple nested objects.
    #'
    #' @return [Learner].
    base_learner = function(recursive = Inf) {
      if (exists(".base_learner", envir = private, inherits = FALSE)) {
        private$.base_learner(recursive)
      } else {
        self
      }
    },

    #' @description
    #' Sets the encapsulation method and fallback learner for the train and predict steps.
    #' There are currently four different methods implemented:
    #'
    #' * `"none"`: Just runs the learner in the current session and measures the elapsed time.
    #'   Does not keep a log, output is printed directly to the console.
    #'   Works well together with [traceback()].
    #' * `"try"`: Similar to `"none"`, but catches error.
    #'   Output is printed to the console and not logged.
    #' * `"evaluate"`: Uses the package \CRANpkg{evaluate} to call the learner, measure time and do the logging.
    #' * `"callr"`: Uses the package \CRANpkg{callr} to call the learner, measure time and do the logging.
    #'   This encapsulation spawns a separate R session in which the learner is called.
    #'   While this comes with a considerable overhead, it also guards your session from being teared down by segfaults.
    #'
    #' The fallback learner is fitted to create valid predictions in case that either the model fitting or the prediction of the original learner fails.
    #' If the training step or the predict step of the original learner fails, the fallback is used to make the predictions.
    #' If the original learner only partially fails during predict step (usually in the form of missing to predict some observations or producing some `NA` predictions), these missing predictions are imputed by the fallback.
    #' Note that the fallback is always trained, as we do not know in advance whether prediction will fail.
    #' If the training step fails, the `$model` field of the original learner is `NULL`.
    #'
    #' Also see the section on error handling the mlr3book:
    #' \url{https://mlr3book.mlr-org.com/chapters/chapter10/advanced_technical_aspects_of_mlr3.html#sec-error-handling}
    #'
    #' @param method `character(1)`\cr
    #'  One of `"none"`, `"try"`, `"evaluate"` or `"callr"`.
    #'  See the description for details.
    #' @param fallback [Learner]\cr
    #'  The fallback learner for failed predictions.
    #'
    #' @return `self` (invisibly).
    encapsulate = function(method, fallback = NULL) {
      assert_choice(method, c("none", "try", "evaluate", "callr"))

      if (method != "none") {
        assert_learner(fallback, task_type = self$task_type)

        if (!identical(self$predict_type, fallback$predict_type)) {
          warningf("The fallback learner '%s' and the base learner '%s' have different predict types: '%s' != '%s'.",
            fallback$id, self$id, fallback$predict_type, self$predict_type)
        }

        # check properties
        properties = intersect(self$properties, c("twoclass", "multiclass", "missings", "importance", "selected_features"))
        missing_properties = setdiff(properties, fallback$properties)

        if (length(missing_properties)) {
          warningf("The fallback learner '%s' does not have the following properties of the learner '%s': %s.",
            fallback$id, self$id, str_collapse(missing_properties))
        }
      } else if (method == "none" && !is.null(fallback)) {
        stopf("Fallback learner must be `NULL` if encapsulation is set to `none`.")
      }

      private$.encapsulation = c(train = method, predict = method)
      private$.fallback = fallback

      return(invisible(self))
    },

    #' @description
    #' Sets parameter values and fields of the learner.
    #' All arguments whose names match the name of a parameter of the [paradox::ParamSet] are set as parameters.
    #' All remaining arguments are assumed to be regular fields.
    #'
    #' @param ... (named `any`)\cr
    #'   Named arguments to set parameter values and fields.
    #' @param .values (named `any`)\cr
    #'   Named list of parameter values and fields.
    configure = function(..., .values = list()) {
      dots = list(...)
      assert_list(dots, names = "unique")
      assert_list(.values, names = "unique")
      assert_disjunct(names(dots), names(.values))
      new_values = insert_named(dots, .values)

      # set params in ParamSet
      if (length(new_values)) {
        param_ids = self$param_set$ids()
        ii = names(new_values) %in% param_ids
        if (any(ii)) {
          self$param_set$values = insert_named(self$param_set$values, new_values[ii])
          new_values = new_values[!ii]
        }
      } else {
        param_ids = character()
      }

      # remaining args go into fields
      if (length(new_values)) {
        ndots = names(new_values)
        for (i in seq_along(new_values)) {
          nn = ndots[[i]]
          if (!exists(nn, envir = self, inherits = FALSE)) {
            stopf("Cannot set argument '%s' for '%s' (not a parameter, not a field).%s",
              nn, class(self)[1L], did_you_mean(nn, c(param_ids, setdiff(names(self), ".__enclos_env__")))) # nolint
          }
          self[[nn]] = new_values[[i]]
        }
      }

      return(invisible(self))
    },

    #' @description
    #' Returns the features selected by the model.
    #' The field `selected_features_impute` controls the behavior if the learner does not support feature selection.
    #' If set to `"error"`, an error is thrown, otherwise all features are returned.
    selected_features = function() {
      if (is.null(self$model)) {
        stopf("No model stored")
      }
      if (private$.selected_features_impute == "error") {
        stopf("Learner does not support feature selection")
      } else {
        self$state$feature_names
      }
    }
  ),

  active = list(
    #' @field use_weights (`character(1)`)\cr
    #' How weights should be handled.
    #' Settings are `"use"` `"ignore"`, and `"error"`.
    #'
    #' * `"use"`: use weights, as supported by the underlying `Learner`.
    #'   Only available for `Learner`s with the property `"weights"`.
    #' * `"ignore"`: do not use weights.
    #' * `"error"`: throw an error if weights are present in the training `Task`.
    #'
    #' For `Learner`s with the property `"weights"`, this is initialized as `"use"`.
    #' For `Learner`s that do not support weights, i.e. without the `"weights"` property, this is initialized as `"error"`.
    #' The latter behavior is to avoid cases where a user erroneously assumes that a `Learner` supports weights when it does not.
    #' For `Learner`s that do not support weights, `use_weights` needs to be set to `"ignore"` if tasks with weights should be handled (by dropping the weights).
    #' See Section 'weights' for more details.
    use_weights = function(rhs) {
      if (!missing(rhs)) {
        assert_choice(rhs, c(if ("weights" %in% self$properties) "use", "ignore", "error"))
        private$.use_weights = rhs
      }
      private$.use_weights
    },

    #' @field data_formats (`character()`)\cr
    #' Supported data format. Always `"data.table"`..
    #' This is deprecated and will be removed in the future.
    data_formats = deprecated_binding("Learner$data_formats", "data.table"),


    #' @field model (any)\cr
    #' The fitted model. Only available after `$train()` has been called.
    model = function(rhs) {
      if (!missing(rhs)) {
        self$state$model = rhs
      }
      self$state$model
    },

    #' @field timings (named `numeric(2)`)\cr
    #' Elapsed time in seconds for the steps `"train"` and `"predict"`.
    #'
    #' When predictions for multiple predict sets were made during [resample()] or [benchmark()],
    #' the predict time shows the cumulative duration of all predictions.
    #' If `learner$predict()` is called manually, the last predict time gets overwritten.
    #'
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


    #' @field hash (`character(1)`)\cr
    #' Hash (unique identifier) for this object.
    #' The hash is calculated based on the learner id, the parameter settings, the predict type, the fallback hash, the parallel predict setting, the validate setting, and the predict sets.
    hash = function(rhs) {
      assert_ro_binding(rhs)
      calculate_hash(class(self), self$id, self$param_set$values, private$.predict_type,
        self$fallback$hash, self$parallel_predict, get0("validate", self), self$predict_sets, private$.use_weights)
    },

    #' @field phash (`character(1)`)\cr
    #' Hash (unique identifier) for this partial object, excluding some components which are varied systematically during tuning (parameter values).
    phash = function(rhs) {
      assert_ro_binding(rhs)
      calculate_hash(class(self), self$id, private$.predict_type,
        self$fallback$hash, self$parallel_predict, get0("validate", self), private$.use_weights)
    },

    #' @field predict_type (`character(1)`)\cr
    #' Stores the currently active predict type, e.g. `"response"`.
    #' Must be an element of `$predict_types`.
    #' A few learners already use the predict type during training.
    #' So there is no guarantee that changing the predict type after training will have any effect or does not lead to errors.
    predict_type = function(rhs) {
      if (missing(rhs)) {
        return(private$.predict_type)
      }

      assert_string(rhs, .var.name = "predict_type")
      if (rhs %nin% self$predict_types) {

        stopf("Learner '%s' does not support predict type '%s'", self$id, rhs)
      }
      private$.predict_type = rhs
    },

    #' @template field_param_set
    param_set = function(rhs) {
      if (!missing(rhs) && !identical(rhs, private$.param_set)) {
        stopf("param_set is read-only.")
      }
      private$.param_set
    },

    #' @field fallback ([Learner])\cr
    #' Returns the fallback learner set with `$encapsulate()`.
    fallback = function(rhs) {
      assert_ro_binding(rhs)
      return(private$.fallback)
    },

    #' @field encapsulation (`character(2)`)\cr
    #' Returns the encapsulation settings set with `$encapsulate()`.
    encapsulation = function(rhs) {
      assert_ro_binding(rhs)
      return(private$.encapsulation)
    },

    #' @field hotstart_stack ([HotstartStack])\cr.
    #' Stores `HotstartStack`.
    hotstart_stack = function(rhs) {
      if (missing(rhs)) {
        return(private$.hotstart_stack)
      }
      assert_r6(rhs, "HotstartStack", null.ok = TRUE)
      private$.hotstart_stack = rhs
    },

    #' @field selected_features_impute (`character(1)`)\cr
    #' Controls the behavior if the learner does not support feature selection.
    #' If set to `"error"`, an error is thrown.
    #' If set to `"all"` the complete feature set is returned.
    selected_features_impute = function(rhs) {
      if (missing(rhs)) {
        return(private$.selected_features_impute)
      }
      private$.selected_features_impute = assert_choice(rhs, c("error", "all"))
    },

    #' @field predict_types (`character()`)\cr
    #' Stores the possible predict types the learner is capable of.
    #' A complete list of candidate predict types, grouped by task type, is stored in [`mlr_reflections$learner_predict_types`][mlr_reflections].
    #' This field is read-only.
    predict_types = function(rhs) {
      assert_ro_binding(rhs)
      return(private$.predict_types)
    }
  ),

  private = list(
    .use_weights = NULL,
    .encapsulation = c(train = "none", predict = "none"),
    .fallback = NULL,
    .predict_type = NULL,
    .predict_types = NULL,
    .param_set = NULL,
    .hotstart_stack = NULL,
    .selected_features_impute = "error",

    # retrieve weights from a task, if it has weights and if the user did not
    # deactivate weight usage through `self$use_weights`.
    # - `task`: Task to retrieve weights from
    # - `no_weights_val`: Value to return if no weights are found (default NULL)
    # return: Numeric vector of weights or `no_weights_val` (default NULL)
    .get_weights = function(task, no_weights_val = NULL) {
      if ("weights" %nin% self$properties) {
        stop("private$.get_weights should not be used in Learners that do not have the 'weights' property.")
      }
      if (self$use_weights == "use" && "weights_learner" %in% task$properties) {
        task$weights_learner$weight
      } else {
        no_weights_val
      }
    },

    deep_clone = function(name, value) {
      switch(name,
        .param_set = value$clone(deep = TRUE),
        .fallback = if (is.null(value)) NULL else value$clone(deep = TRUE),
        state = {
          if (!is.null(value$train_task)) {
            value$train_task = value$train_task$clone(deep = TRUE)
          }
          value$log = copy(value$log)
          value
        },
        value
      )
    }
  )
)

#' @export
rd_info.Learner = function(obj, ...) {
  x = c("",
    sprintf("* Task type: %s", rd_format_string(obj$task_type)),
    sprintf("* Predict Types: %s", rd_format_string(obj$predict_types)),
    sprintf("* Feature Types: %s", rd_format_string(obj$feature_types)),
    sprintf("* Required Packages: %s", rd_format_packages(obj$packages))
  )
  paste(x, collapse = "\n")
}

get_log_condition = function(state, condition) {
  if (is.null(state$log)) {
    character()
  } else {
    fget(state$log, i = condition, j = "msg", key = "class")
  }
}

#' @export
default_values.Learner = function(x, search_space, task, ...) { # nolint
  values = default_values(x$param_set)

  if (any(search_space$ids() %nin% names(values))) {
    stopf("Could not find default values for the following parameters: %s",
      str_collapse(setdiff(search_space$ids(), names(values))))
  }

  values[search_space$ids()]
}
# #' @export
# format_list_item.Learner = function(x, ...) { # nolint
#   sprintf("<lrn:%s>", x$id)
# }


#' @export
marshal_model.learner_state = function(model, inplace = FALSE, ...) {
  if (is.null(model$model)) {
    return(model)
  }
  mm = marshal_model(model$model, inplace = inplace, ...)
  if (!is_marshaled_model(mm)) {
    return(model)
  }
  model$model = mm
  structure(list(
    marshaled = model,
    packages = "mlr3"
  ), class = c("learner_state_marshaled", "list_marshaled", "marshaled"))
}

#' @export
unmarshal_model.learner_state_marshaled = function(model, inplace = FALSE, ...) {
  mm = model$marshaled
  mm$model = unmarshal_model(mm$model, inplace = inplace, ...)
  return(mm)
}

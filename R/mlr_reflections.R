#' @title Reflections for mlr3
#'
#' @format [environment].
#' @description
#' Environment which stores various information to allow objects to examine and introspect their structure and properties
#' (c.f. [Reflections](https://www.wikiwand.com/en/Reflection_(computer_programming))).
#'
#' This environment be modified by third-party packages, e.g. by adding information about new task types
#' or by extending the set of allowed feature types.
#'
#' Third-party packages that modify the reflections must register themselves in the `loaded_packages` field.
#'
#' The following objects are set by \CRANpkg{mlr3}:
#'
#' * `task_types` (`data.table()`)\cr
#'   Table with task type (`"type"`), the implementing package (`"pkg"`), and the names of the generators
#'   of the corresponding [Task] (`"task"`), [Learner] (`"learner"`),
#'   [Prediction] (`"prediction"`), [Measure] (`"measure"`) and fallback [Learner].
#'
#' * `task_feature_types` (named `character()`)\cr
#'   Vector of base R types supported as [Task] features, named with a 3 letter abbreviation.
#'
#' * `task_row_roles` (`character()`)\cr
#'   Vector of supported row roles for a [Task].
#'
#' * `task_col_roles` (list of `character()`)\cr
#'   List of vectors of supported column roles for a [Task], named by their task type.
#'
#' * `task_properties` (list of `character()`)\cr
#'   List of vectors of supported [Task] properties, named by their task type.
#'
#' * `task_mandatory_properties` (list of `character()`)\cr
#'   List of vectors of [Task] properties which necessarily must be supported by the [Learner].
#'   I.e., if the task property is not found in the set of the learner properties, an exception
#'   is raised.
#'
#' * `task_print_col_roles` (list of named `character()`)\cr
#'   Vector of column roles to print via `print(task)` if the role is not empty, either
#'   before or after the task's target, properties and features.
#'   The names of the column roles are the values, the names correspond to the labels to use in the printer.
#'
#' * `learner_properties` (list of `character()`)\cr
#'   List of vectors of supported [Learner] properties, named by their task type.
#'
#' * `learner_predict_types` (list of list of `character()`)\cr
#'   List of lists of supported [Learner] predict_types, named by their task type.
#'   The inner list translates the `"predict_type"` to all predict types returned, e.g.
#'   predict type `"prob"` for a [LearnerClassif] provides the probabilities as well as the predicted labels, therefore `"prob"` maps to `c("response", "prob")`.
#'
#' * `learner_predict_types` (list of list of `character()`)\cr
#'   List of lists of supported [Learner] predict_types, named by their task type.
#'
#' * `learner_param_tags` (`character()`)\cr
#'   Character vector of allowed 'tags' for the [paradox::ParamSet]s of a [Learner].
#'
#' * `predict_sets` (`character()`)\cr
#'   Vector of possible predict sets. Currently supported are `"train"`, `"test"` and `"holdout"`.
#'
#' * `measure_properties` (list of `character()`)\cr
#'   List of vectors of supported [Measure] properties, named by their task type.
#'
#' * `default_measures` (list of `character()`)\cr
#'   List of keys for the default [Measure]s, named by their task type.
#'
#' * `rr_names` (`character()`)\cr
#'   Names of the objects stored in a [ResampleResult].
#'
#' * `auto_converters` (`environment()`)\cr
#'   Environment of converter functions used for `rbind`-ing data to tasks.
#'   Functions are named using the pattern `"[from_type]___[to_type]"`.
#'   Can be extended by third-party with additional converters.
#'
#' @keywords internal
#' @export
#' @examples
#' ls.str(mlr_reflections)
mlr_reflections = new.env(parent = emptyenv())

local({
  ### Task
  # task types + constructors
  mlr_reflections$task_types = rowwise_table(.key = "type",
    ~type,          ~package, ~task,              ~learner,         ~prediction,          ~prediction_data,         ~measure,
    "regr",         "mlr3",   "TaskRegr",         "LearnerRegr",    "PredictionRegr",     "PredictionDataRegr",     "MeasureRegr",
    "classif",      "mlr3",   "TaskClassif",      "LearnerClassif", "PredictionClassif",  "PredictionDataClassif",  "MeasureClassif",
    "unsupervised", "mlr3",   "TaskUnsupervised", "Learner",        NA_character_,        NA_character_,            NA_character_
  )

  mlr_reflections$task_feature_types = c(
    lgl = "logical", int = "integer", dbl = "numeric", chr = "character", fct = "factor", ord = "ordered", pxc = "POSIXct", dte = "Date"
  )

  mlr_reflections$task_row_roles = c(
    "use"
  )

  tmp = c("feature", "target", "name", "order", "stratum", "group", "offset", "weights_learner", "weights_measure")
  mlr_reflections$task_col_roles = list(
    regr = tmp,
    classif = tmp,
    unsupervised = c("feature", "name", "order")
  )

  tmp = c("strata", "groups", "offset", "weights_learner", "weights_measure")
  mlr_reflections$task_properties = list(
    classif = c(tmp, "twoclass", "multiclass"),
    regr = tmp,
    unsupervised = character()
  )

  mlr_reflections$task_mandatory_properties = list(
    classif = c("twoclass", "multiclass")
  )

  mlr_reflections$task_print_col_roles = list(
    before = character(),
    after = c("Order by" = "order", "Strata" = "stratum", "Groups" = "group", "Offset" = "offset", "Weights/Learner" = "weights_learner", "Weights/Measure" = "weights_measure")
  )

  ### Learner
  tmp = c("featureless", "missings", "weights", "importance", "selected_features", "oob_error", "hotstart_forward", "hotstart_backward", "validation", "internal_tuning", "marshal", "offset")
  mlr_reflections$learner_properties = list(
    classif = c(tmp, "twoclass", "multiclass"),
    regr = tmp
  )

  mlr_reflections$learner_predict_types = list(
    classif = list(response = "response", prob = c("response", "prob")),
    regr = list(response = "response", se = c("response", "se"), quantiles = c("response", "quantiles"), distr = c("response", "se", "distr"))
  )

  # Allowed tags for parameters
  mlr_reflections$learner_param_tags = c("train", "predict", "hotstart", "importance", "threads", "required", "internal_tuning")

  ### Prediction
  mlr_reflections$predict_sets = c("train", "test", "internal_valid")

  ### Resampling
  mlr_reflections$resampling_properties = c("duplicated_ids", "weights")

  ### Measures
  tmp = c("na_score", "requires_task", "requires_learner", "requires_model", "requires_train_set", "weights", "primary_iters", "requires_no_prediction")
  mlr_reflections$measure_properties = list(
    classif = tmp,
    regr = tmp
  )

  mlr_reflections$default_measures = list(
    classif = "classif.ce",
    regr = "regr.mse",
    unsupervised = NA_character_
  )

  ### ResampleResult
  mlr_reflections$rr_names = c("task", "learner", "resampling", "iteration")

  ### Cached package version
  mlr_reflections$package_version = packageVersion("mlr3")

  ### Loaded packages
  mlr_reflections$loaded_packages = "mlr3"
})

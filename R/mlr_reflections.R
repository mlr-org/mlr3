#' @title Reflections for mlr3
#'
#' @description Environment which stores various information for reflections.
#' @keywords internal
#' @export
#' @examples
#' ls.str(mlr_reflections)
mlr_reflections = new.env(parent = emptyenv())


local({
  ### options
  mlr_reflections$mlr_control_defaults = list(
    store_model = FALSE,
    encapsulate_train = "none",
    encapsulate_predict = "none",
    log_threshold = 400L
  )

  ### Task
  mlr_reflections$task_types = c(
    "regr", "classif"
  )

  mlr_reflections$task_feature_types = c(
    lgl = "logical", int = "integer", dbl = "numeric", chr = "character", fct = "factor", ord = "ordered"
  )

  mlr_reflections$task_row_roles = c(
    "use", "validation"
  )

  tmp = c("feature", "target", "label", "order", "groups", "weights")
  mlr_reflections$task_col_roles = list(
    regr = tmp,
    classif = tmp
  )

  tmp = c("weights", "groups")
  mlr_reflections$task_properties = list(
    classif = c(tmp, "twoclass", "multiclass"),
    regr = tmp
  )

  mlr_reflections$task_data_formats = c(
    "data.table", "Matrix"
  )


  ### Learner
  tmp = c("missings", "weights", "parallel", "importance", "selected_features", "oob_error")
  mlr_reflections$learner_properties = list(
    classif = c(tmp, "twoclass", "multiclass"),
    regr = tmp
  )

  mlr_reflections$learner_predict_types = list(
    classif = list(response = "response", prob = c("response", "prob")),
    regr = list(response = "response", se = c("response", "se"))
  )

  ### Measures
  mlr_reflections$default_measures = list(
    classif = "classif.ce",
    regr = "regr.mse"
  )

  ### ResampleResult
  mlr_reflections$rr_names = c("task", "learner", "resampling", "iteration", "prediction")

  ### Log
  mlr_reflections$log_classes = c("output", "warning", "error")
})

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
    store_prediction = TRUE,
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


  ### Experiment
  mlr_reflections$experiment_states = c("undefined", "defined", "trained", "predicted", "scored")

  mlr_reflections$experiment_slots = rowwise_table(
    ~name,             ~type,        ~atomic, ~state,
    "task",            "Task",       FALSE,   "defined",
    "learner",         "Learner",    FALSE,   "defined",
    "resampling",      "Resampling", FALSE,   "trained",
    "iteration",       "integer",    TRUE,    "trained",
    "train_log",       "data.table", FALSE,   "trained",
    "train_time",      "numeric",    TRUE,    "trained",
    "predict_log",     "data.table", FALSE,   "predicted",
    "predict_time",    "numeric",    TRUE,    "predicted",
    "prediction_data", "list",       FALSE,   "predicted",
    "measures",        "list",       FALSE,   "scored",
    "performance",     "list",       FALSE,   "scored",
    "score_time",      "numeric",    TRUE,    "scored"
  )

  mlr_reflections$experiment_slots$state = ordered(mlr_reflections$experiment_slots$state, levels = mlr_reflections$experiment_states)


  ### Log
  mlr_reflections$log_classes = c("output", "warning", "error")
})

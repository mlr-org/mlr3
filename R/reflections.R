#' @title Reflections for mlr3
#'
#' @description
#' Environment which stores various information for reflections.
#' @keywords internal
#' @export
mlr_reflections = new.env(parent = emptyenv())

mlr_reflections$backend_formats = c(
  "data.table", "sparse"
)

mlr_reflections$task_types = c(
  "regr", "classif"
)

mlr_reflections$task_feature_types = c(
  "logical", "integer", "numeric", "character", "factor", "ordered"
)

mlr_reflections$task_row_roles = c(
  "use", "validation"
)

mlr_reflections$task_col_roles = c(
  "feature", "target", "order"
)

mlr_reflections$task_properties = list(
  classif = c("weights", "twoclass", "multiclass"),
  regr    = c("weights")
)

mlr_reflections$learner_properties = list(
  classif = c("missings", "weights", "parallel", "twoclass", "multiclass"),
  regr    = c("missings", "weights", "parallel")
)

mlr_reflections$predict_types = list(
  classif = c("response", "prob"),
  regr    = c("response", "se")
)

mlr_reflections$experiment_slots = data.table(
  name =   c("task",    "learner", "resampling", "iteration", "model",       "train_log",  "train_time", "predict_log", "predict_time", "prediction", "measures", "performance", "score_time"),
  type =   c("Task",    "Learner", "Resampling", "integer",   NA_character_, "data.table", "numeric",    "data.table",  "numeric",      "data.table", "list",     "list",        "numeric"),
  atomic = c(FALSE,     FALSE,     FALSE,        TRUE,        FALSE,         FALSE,        TRUE,         FALSE,         TRUE,           FALSE,        FALSE,      FALSE,         TRUE),
  state =  c("defined", "defined", "trained",    "trained",   "trained",     "trained",    "trained",    "predicted",   "predicted",    "predicted",  "scored",   "scored",      "scored")
)

mlr_reflections$experiment_slots$state = ordered(mlr_reflections$experiment_slots$state, levels = c("defined", "trained", "predicted", "scored"))

mlr_reflections$log_classes = c("output", "message", "warning", "error")

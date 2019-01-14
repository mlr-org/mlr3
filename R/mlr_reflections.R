#' @title Reflections for mlr3
#'
#' @description Environment which stores various information for reflections.
#' @keywords internal
#' @export
#' @examples
#' ls.str(mlr_reflections)
mlr_reflections = new.env(parent = emptyenv())

mlr_reflections$backend_formats = c(
  "data.table", "sparse"
)

mlr_reflections$task_types = c(
  "regr", "classif"
)

mlr_reflections$task_feature_types = c(
  lgl = "logical", int = "integer", dbl = "numeric", chr = "character", fct = "factor", ord = "ordered"
)

mlr_reflections$task_row_roles = c(
  "use", "validation"
)

mlr_reflections$task_col_roles = list(
  regr = c("feature", "target", "order", "groups", "weights"),
  classif = c("feature", "target", "order", "groups", "weights")
)

mlr_reflections$task_properties = list(
  classif = c("weights", "twoclass", "multiclass"),
  regr    = c("weights")
)

mlr_reflections$learner_properties = list(
  classif = c("missings", "weights", "parallel", "twoclass", "multiclass", "importance"),
  regr    = c("missings", "weights", "parallel", "importance")
)

mlr_reflections$predict_types = list(
  classif = c("response", "prob"),
  regr    = c("response", "se")
)

mlr_reflections$experiment_states = c("undefined", "defined", "trained", "predicted", "scored")

mlr_reflections$experiment_slots = data.table(
  name =   c("task",    "learner", "resampling", "iteration", "train_log",  "train_time", "predict_log", "predict_time", "prediction", "measures", "performance", "score_time"),
  type =   c("Task",    "Learner", "Resampling", "integer",   "data.table", "numeric",    "data.table",  "numeric",      "data.table", "list",     "list",        "numeric"),
  atomic = c(FALSE,     FALSE,     FALSE,        TRUE,        FALSE,        TRUE,         FALSE,         TRUE,           FALSE,        FALSE,      FALSE,         TRUE),
  state =  c("defined", "defined", "trained",    "trained",   "trained",    "trained",    "predicted",   "predicted",    "predicted",  "scored",   "scored",      "scored")
)

mlr_reflections$experiment_slots$state = ordered(mlr_reflections$experiment_slots$state, levels = mlr_reflections$experiment_states)

mlr_reflections$log_classes = c("output", "warning", "error")

mlr_reflections$default_mlr_control = list(
  store_model = TRUE,
  store_prediction = TRUE,
  encapsulate_train = "none",
  encapsulate_predict = "none",
  log_threshold = INFO
)

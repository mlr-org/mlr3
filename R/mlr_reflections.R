#' @title Reflections for mlr3
#'
#' @description Environment which stores various information for reflections.
#' @keywords internal
#' @export
#' @examples
#' ls.str(mlr_reflections)
mlr_reflections = new.env(parent = emptyenv())


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

mlr_reflections$task_col_roles = list(
  regr = c("feature", "target", "label", "order", "groups", "weights"),
  classif = c("feature", "target", "label", "order", "groups", "weights")
)

mlr_reflections$task_properties = list(
  classif = c("weights", "groups", "twoclass", "multiclass"),
  regr    = c("weights", "groups")
)

mlr_reflections$task_data_formats = c(
  "data.table", "Matrix"
)


### Learner
mlr_reflections$learner_properties = list(
  classif = c("missings", "weights", "parallel", "twoclass", "multiclass", "importance", "selected_features"),
  regr    = c("missings", "weights", "parallel", "importance", "selected_features")
)

mlr_reflections$learner_predict_types = list(
  classif = c("response", "prob"),
  regr    = c("response", "se")
)

mlr_reflections$experiment_states = c("undefined", "defined", "trained", "predicted", "scored")

mlr_reflections$experiment_slots = dribble(
  ~name,          ~type,        ~atomic, ~state,
  "task",         "Task",       FALSE,   "defined",
  "learner",      "Learner",    FALSE,   "defined",
  "resampling",   "Resampling", FALSE,   "trained",
  "iteration",    "integer",    TRUE,    "trained",
  "train_log",    "data.table", FALSE,   "trained",
  "train_time",   "numeric",    TRUE,    "trained",
  "predict_log",  "data.table", FALSE,   "predicted",
  "predict_time", "numeric",    TRUE,    "predicted",
  "prediction",   "data.table", FALSE,   "predicted",
  "measures",     "list",       FALSE,   "scored",
  "performance",  "list",       FALSE,   "scored",
  "score_time",   "numeric",    TRUE,    "scored"
)

mlr_reflections$experiment_slots$state = ordered(mlr_reflections$experiment_slots$state, levels = mlr_reflections$experiment_states)

### Log

mlr_reflections$log_classes = c("output", "warning", "error")

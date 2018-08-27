capabilities = new.env(parent = emptyenv())

capabilities$task_types = c(
  # FIXME: do not use abbreviations?
  "regr", "classif", "forecasting", "survival"
)

capabilities$task_col_types = c(
  "logical", "integer", "numeric", "character", "factor", "ordered"
)

capabilities$task_col_roles = c(
  "primary_key", "feature", "target", "ignore"
)

capabilities$task_row_roles = c(
  "training", "evaluation", "ignore"
)

capabilities$learner_props = list(
  any = c(sprintf("feat.%s", capabilities$task_col_types), "missings", "weights", "parallel")
)
capabilities$learner_props$classif = c(capabilities$learner_props$any, "twoclass", "multiclass", "prob")
capabilities$learner_props$regr = c(capabilities$learner_props$any, "se")

capabilities$predict_types = list(
  classif = c("response", "prob"),
  regr    = c("response", "se")
)

capabilities$experiment_slots = data.table(
  name = c("task",  "learner", "resampling", "iteration", "train_log",  "train_time", "model",      "test_log",   "test_time", "predicted", "performance"),
  type = c("Task",  "Learner", "Resampling", "integer",   "data.table", "numeric",    NA_character_, "data.table", "numeric",   "vector",    "list"),
  atomic = c(FALSE, FALSE,     FALSE,        TRUE,        FALSE,        TRUE,         FALSE,         FALSE,        TRUE,        FALSE,       FALSE)
)

capabilities$experiment_states = c(
  c("defined", "trained", "predicted", "scored")
)

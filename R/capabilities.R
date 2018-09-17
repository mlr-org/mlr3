capabilities = new.env(parent = emptyenv())

capabilities$task_types = c(
  "TaskSupervised", "TaskRegr", "TaskClassif"
)

capabilities$task_col_types = c(
  "logical", "integer", "numeric", "character", "factor", "ordered"
)

capabilities$task_row_roles = c(
  "use", "validation", "ignore"
)

capabilities$task_col_roles = c(
  "primary_key", "feature", "target", "ignore"
)

capabilities$learner_props = list(
  Task = c(sprintf("feat.%s", capabilities$task_col_types), "missings", "weights", "parallel")
)
capabilities$learner_props$TaskClassif = c(capabilities$learner_props$Task, "twoclass", "multiclass", "prob")
capabilities$learner_props$TaskRegr = c(capabilities$learner_props$Task, "se")

capabilities$predict_types = list(
  classif = c("response", "prob"),
  regr    = c("response", "se")
)

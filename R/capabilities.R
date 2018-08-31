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
  any = c(sprintf("feat.%s", capabilities$task_col_types), "missings", "weights", "parallel")
)
capabilities$learner_props$classif = c(capabilities$learner_props$any, "twoclass", "multiclass", "prob")
capabilities$learner_props$regr = c(capabilities$learner_props$any, "se")

capabilities$predict_types = list(
  classif = c("response", "prob"),
  regr    = c("response", "se")
)

capabilities$experiment_states = c(
  c("defined", "trained", "predicted", "scored")
)

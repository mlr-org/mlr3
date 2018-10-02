capabilities = new.env(parent = emptyenv())

capabilities$task_types = c(
  "any", "regr", "classif"
)

capabilities$task_feature_types = c(
  "logical", "integer", "numeric", "character", "factor", "ordered"
)

capabilities$task_row_roles = c(
  "use", "validation", "ignore"
)

capabilities$task_col_roles = c(
  "primary_key", "feature", "target", "ignore"
)

capabilities$learner_props = list(
  task = c(sprintf("feat.%s", capabilities$task_feature_types), "missings", "weights", "parallel")
)
capabilities$learner_props$classif = c(capabilities$learner_props$task, "twoclass", "multiclass", "prob")
capabilities$learner_props$regr = c(capabilities$learner_props$task, "se")

capabilities$predict_types = list(
  classif = c("response", "prob"),
  regr    = c("response", "se")
)

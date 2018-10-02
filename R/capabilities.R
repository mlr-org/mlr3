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
  classif = c("missings", "weights", "parallel", "twoclass", "multiclass"),
  regr    = c("missings", "weights", "parallel")
)

capabilities$predict_types = list(
  classif = c("response", "prob"),
  regr    = c("response", "se")
)

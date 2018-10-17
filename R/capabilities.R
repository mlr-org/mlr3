capabilities = new.env(parent = emptyenv())

capabilities$task_types = c(
  "regr", "classif"
)

capabilities$task_feature_types = c(
  "logical", "integer", "numeric", "character", "factor", "ordered"
)

capabilities$task_row_roles = c(
  "use", "validation"
)

capabilities$task_col_roles = c(
  "feature", "target", "order"
)

capabilities$task_properties = list(
  classif = c("weights", "twoclass", "multiclass"),
  regr    = c("weights")
)

capabilities$learner_properties = list(
  classif = c("missings", "weights", "parallel", "twoclass", "multiclass"),
  regr    = c("missings", "weights", "parallel")
)

capabilities$predict_types = list(
  classif = c("response", "prob"),
  regr    = c("response", "se")
)

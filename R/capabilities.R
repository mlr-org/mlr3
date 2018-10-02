capabilities = new.env(parent = emptyenv())

capabilities$task_types = c(
  "regr", "classif"
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

capabilities$task_properties = list(
  classif = c("weights", "twoclass", "multiclass"),
  regr    = c("weights")
)

capabilities$learner_properties = list(
  classif = c("missings", "weights", "parallel", "twoclass", "multiclass"),
  regr    = c("missings", "weights", "parallel")
)

capabilities$predict_types = list(
  classif = c(NA_character_, "response", "prob"),
  regr    = c(NA_character_, "response", "se")
)

capabilities = new.env(parent = emptyenv())

capabilities$task.types = c(
  # FIXME: do not use abbreviations?
  "regr", "classif", "forecasting", "survival"
)

capabilities$task.col.types = c(
  "logical", "integer", "numeric", "character", "factor", "ordered"
)

capabilities$task.col.roles = c(
  "primary.key", "feature", "target", "ignore"
)

capabilities$task.row.roles = c(
  "training", "evaluation", "ignore"
)

capabilities$learner.props = list(
  any = c(sprintf("feat.%s", capabilities$task.col.types), "missings", "weights", "parallel")
)
capabilities$learner.props$classif = c(capabilities$learner.props$any, "twoclass", "multiclass", "prob")
capabilities$learner.props$regr = c(capabilities$learner.props$any, "se")

capabilities$predict.types = list(
  classif = c("response", "prob"),
  regr    = c("response", "se")
)

capabilities$experiment.slots = data.table(
  name = c("task",  "learner", "resampling", "iteration", "train.log",  "train.time", "model",      "test.log",   "test.time", "predicted", "performance"),
  type = c("Task",  "Learner", "Resampling", "integer",   "data.table", "numeric",    NA_character_, "data.table", "numeric",   "vector",    "list"),
  atomic = c(FALSE, FALSE,     FALSE,        TRUE,        FALSE,        TRUE,         FALSE,         FALSE,        TRUE,        FALSE,       FALSE)
)

capabilities$experiment.states = c(
  c("defined", "trained", "predicted", "scored")
)

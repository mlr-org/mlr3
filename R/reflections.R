reflections = new.env(parent = emptyenv())

reflections$experiment_slots = data.table(
  name =   c("task",    "learner", "resampling", "iteration", "model",       "train_log",  "train_time", "predict_log", "predict_time", "prediction", "measures", "performance", "score_time"),
  type =   c("Task",    "Learner", "Resampling", "integer",   NA_character_, "data.table", "numeric",    "data.table",  "numeric",      "data.table", "list",     "list",        "numeric"),
  atomic = c(FALSE,     FALSE,     FALSE,        TRUE,        FALSE,         FALSE,        TRUE,         FALSE,         TRUE,           FALSE,        FALSE,      FALSE,         TRUE),
  state =  c("defined", "defined", "trained",    "trained",   "trained",     "trained",    "trained",    "predicted",   "predicted",    "predicted",  "scored",   "scored",      "scored")
)
reflections$experiment_slots$state = ordered(reflections$experiment_slots$state, levels = c("defined", "trained", "predicted", "scored"))

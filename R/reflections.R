reflections = new.env(parent = emptyenv())

reflections$experiment_slots = data.table(
  name = c("task",  "learner", "resampling", "iteration", "train_log",  "train_time", "model",      "test_log",   "test_time", "predicted", "performance"),
  type = c("Task",  "Learner", "Resampling", "integer",   "data.table", "numeric",    NA_character_, "data.table", "numeric",   "vector",    "list"),
  atomic = c(FALSE, FALSE,     FALSE,        TRUE,        FALSE,        TRUE,         FALSE,         FALSE,        TRUE,        FALSE,       FALSE)
)

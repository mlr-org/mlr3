reflections = new.env(parent = emptyenv())

reflections$experiment_slots = data.table(
  name =   c("task", "learner", "resampling", "iteration", "train_log",  "train_time", "test_log",   "test_time", "predicted", "performance"),
  type =   c("Task", "Learner", "Resampling", "integer",   "data.table", "numeric",    "data.table", "numeric",   "vector",    "list"),
  atomic = c(FALSE,  FALSE,     FALSE,        TRUE,        FALSE,        TRUE,         FALSE,        TRUE,        FALSE,       FALSE)
)

split_train_validation = function(task, train.size = 0.9) {
  assertTask(task)
  assertNumber(train.size, lower = 0, upper = 1)

  resampling = mlr.resamplings$get("holdout")
  resampling$ratio = train.size
  instance = resampling$instantiate(task)
  nt = task$clone()
  task$rows[id %in% instance$train.set(1L), role := "training"]
  task$rows[id %in% instance$test.set(1L), role := "validation"]
  task
}

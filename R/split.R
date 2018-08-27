split_train_validation = function(task, train.size = 0.9) {
  assert_task(task)
  assert_number(train.size, lower = 0, upper = 1)

  resampling = mlr_resamplings$get("holdout")
  resampling$ratio = train.size
  instance = resampling$instantiate(task)
  nt = task$clone()
  task$rows[id %in% instance$train_set(1L), role := "training"]
  task$rows[id %in% instance$test_set(1L), role := "validation"]
  task
}

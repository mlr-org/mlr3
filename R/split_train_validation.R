split_train_validation = function(task, train.size = 0.9) {
  assert_task(task)
  assert_number(train.size, lower = 0, upper = 1)

  resampling = mlr_resamplings$get("holdout")
  resampling$param_vals = list(ratio = train.size)
  instance = resampling$instantiate(task)
  id = NULL
  nt = task$clone()
  nt$set_row_role(instance$train_set(1L), "use")
  nt$set_row_role(instance$test_set(1L), "validation")
  nt
}

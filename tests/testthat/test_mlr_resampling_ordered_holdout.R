test_that("ordered holdout has no duplicated ids", {
  r = rsmp("ordered_holdout")
  expect_identical(r$duplicated_ids, FALSE)
})

test_that("order", {
  data(AirPassengers)
  tab = data.frame(Y = as.matrix(AirPassengers), date = as.numeric(time(AirPassengers)))
  tab$x = runif(nrow(tab))
  tab$row_id = c(1001:1100, 1:44)
  backend = as_data_backend(tab, primary_key = "row_id")
  task = TaskRegr$new("ap", backend, target = "Y")
  task$col_roles$order = "date"
  r = rsmp("ordered_holdout")
  r$instantiate(task)

  expect_lt(max(task$data(r$train_set(1))$date), min(task$data(r$test_set(1))$date))
})

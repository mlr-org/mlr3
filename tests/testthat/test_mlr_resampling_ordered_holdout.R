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

test_that("order with posix", {
  data = data.table(
    y = sample(seq(from = Sys.time() - 24 * 3600, to = Sys.time(), length.out = 20)),
    x = runif(20),
    class = factor(sample(c("a", "b"), 20, replace = TRUE), levels = c("a", "b"))
  )
  task = TaskClassif$new("test", data, target = "class")
  task$col_roles$order = "y"
  expect_posixct(task$data()$y, sorted = TRUE)

  r = rsmp("ordered_holdout")$instantiate(task)
  expect_lt(max(task$data(r$train_set(1))$y), min(task$data(r$test_set(1))$y))
})

test_that("order with posix and time unit", {
  data1 = data.table(
    y = seq(from = Sys.time() - 24 * 3600, to = Sys.time(), length.out = 24),
    x = runif(24),
    class = factor(sample(c("a", "b"), 24, replace = TRUE), levels = c("a", "b"))
  )
  data2 = data.table(
    y = seq(from = Sys.time() - 12 * 3600, to = Sys.time(), length.out = 12),
    x = runif(12),
    class = factor(sample(c("a", "b"), 12, replace = TRUE), levels = c("a", "b"))
  )
  data = rbind(data1, data2)

  task = TaskClassif$new("test", data, target = "class")
  task$col_roles$order = "y"
  expect_posixct(task$data()$y, sorted = TRUE)

  r = rsmp("ordered_holdout", unit = "hours", ratio = 0.5)$instantiate(task)
  length(r$train_set(1))
  length(r$test_set(1))

  expect_lt(max(task$data(r$train_set(1))$y), min(task$data(r$test_set(1))$y))




  r = rsmp("ordered_holdout", unit = "hours", n = 5)$instantiate(task)
  length(r$train_set(1))
  length(r$test_set(1))

  r = rsmp("ordered_holdout", unit = "hours", n = -5)$instantiate(task)
  length(r$train_set(1))
  length(r$test_set(1))
})

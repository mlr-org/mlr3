context("mlr_resampling_holdout")

test_that("holdout has no duplicated ids", {
  r = rsmp("holdout")
  expect_identical(r$duplicated_ids, FALSE)
})

test_that("stratification", {
  data = data.table(y = factor(rep(letters[1:2], times = c(90, 10))), x1 = runif(100), x2 = rep(LETTERS[1:2], times = c(50, 50)))
  b = as_data_backend(data)
  task = TaskClassif$new("stratify_data", b, target = "y")
  task$col_roles$stratum = task$target_names

  r = rsmp("holdout", ratio = .5)
  r$instantiate(task)

  i = 1L
  expect_equal(task$data(r$train_set(i))[y == "a", .N], 45)
  expect_equal(task$data(r$train_set(i))[y == "b", .N], 5)
  expect_equal(task$data(r$test_set(i))[y == "a", .N], 45)
  expect_equal(task$data(r$test_set(i))[y == "b", .N], 5)
})

test_that("grouping", {
  r = rsmp("holdout")
  expect_grouping_works(r)
})

test_that("set size is optimal with respect to binomial likelihood", {
  N <- 2
  prop.train <- 0.7
  task <- tsk("iris")$filter(1:N)
  rho <- rsmp("holdout", ratio=prop.train)
  set.seed(1)
  rho$instantiate(task)
  indices <- rho$train_set(1)
  expect_equal(length(indices), 2)
  ## for N=2 items total and p=0.7 probability of being in train set,
  ## the most likely number of items in the train set is 2.
  dbinom(0:N, N, prop.train)
})

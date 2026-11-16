test_that("cv has no duplicated ids", {
  r = rsmp("cv")
  expect_identical(r$duplicated_ids, FALSE)
})

test_that("split into evenly sized groups", {
  task = tsk("iris")
  r = rsmp("cv", folds = 3)
  r$instantiate(task)
  expect_equal(uniqueN(lengths(lapply(1:3, r$train_set))), 1L)
})

test_that("stratification", {
  data = data.table(
    y = factor(rep(letters[1:2], times = c(90, 10))),
    x1 = runif(100),
    x2 = rep(LETTERS[1:2], times = c(50, 50))
  )
  b = as_data_backend(data)
  task = TaskClassif$new("stratify_data", b, target = "y")
  task$col_roles$stratum = task$target_names

  r = rsmp("cv", folds = 5L)
  r$instantiate(task)

  for (i in seq_len(r$iters)) {
    expect_equal(task$data(r$train_set(i))[y == "a", .N], 72)
    expect_equal(task$data(r$train_set(i))[y == "b", .N], 8)
    expect_equal(task$data(r$test_set(i))[y == "a", .N], 18)
    expect_equal(task$data(r$test_set(i))[y == "b", .N], 2)
  }
})

test_that("more folds than observations", {
  task = tsk("iris")$filter(1:3)
  expect_error(
    rsmp("cv", folds = 5L)$instantiate(task),
    "Cannot split 3 observations into 5 folds",
    class = "Mlr3ErrorInput"
  )
  expect_resampling(rsmp("cv", folds = 3L)$instantiate(task), task = task)

  data = data.table(y = rnorm(8), g = rep(1:4, each = 2L))
  task = as_task_regr(data, target = "y")
  task$col_roles$group = "g"
  expect_error(rsmp("cv", folds = 5L)$instantiate(task), "Cannot split 4 groups into 5 folds", class = "Mlr3ErrorInput")

  # small strata are fine, empty folds have an empty test set
  data = data.table(y = rnorm(4), s = c("a", "a", "b", "b"))
  task = as_task_regr(data, target = "y")
  task$col_roles$stratum = "s"
  r = rsmp("cv", folds = 3L)$instantiate(task)
  expect_identical(r$test_set(3L), integer())
  expect_set_equal(r$train_set(3L), 1:4)
  expect_set_equal(unlist(lapply(1:3, r$test_set)), 1:4)
})

test_that("grouping", {
  r = rsmp("cv", folds = 3)
  expect_grouping_works(r)
})

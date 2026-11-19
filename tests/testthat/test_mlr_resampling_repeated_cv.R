test_that("repeated cv has no duplicated ids", {
  r = rsmp("repeated_cv")
  expect_identical(r$duplicated_ids, FALSE)
})

test_that("folds first, then repetitions", {
  task = tsk("iris")
  rrcv = rsmp("repeated_cv", repeats = 2, folds = 3)
  rrcv$instantiate(task)

  expect_set_equal(c(rrcv$train_set(1), rrcv$test_set(1)), task$row_ids)
  expect_integer(intersect(intersect(rrcv$test_set(1), rrcv$test_set(2)), rrcv$test_set(3)), len = 0L)

  expect_equal(rrcv$folds(seq_len(rrcv$iters)), rep(1:3, 2))
  expect_equal(rrcv$repeats(seq_len(rrcv$iters)), rep(1:2, each = 3))
})

test_that("repeats is required", {
  r = rsmp("repeated_cv")
  r$param_set$values$repeats = NULL
  expect_error(r$param_set$get_values(), "repeats")
  expect_error(r$instantiate(tsk("iris")), "repeats")
})

test_that("stratification", {
  data = data.table(
    y = factor(rep(letters[1:2], times = c(90, 10))),
    x1 = runif(100),
    x2 = rep(LETTERS[1:2], times = c(50, 50))
  )
  task = TaskClassif$new("stratify_data", data, target = "y")
  task$col_roles$stratum = task$target_names

  r = rsmp("repeated_cv", folds = 5, repeats = 2)
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
    rsmp("repeated_cv", folds = 5L, repeats = 2L)$instantiate(task),
    "Cannot split 3 observations into 5 folds",
    class = "Mlr3ErrorInput"
  )
})

test_that("stratification keeps the folds evenly sized", {
  data = data.table(y = rnorm(110), s = factor(rep(1:10, each = 11)))
  task = as_task_regr(data, target = "y")
  task$col_roles$stratum = "s"

  r = rsmp("repeated_cv", folds = 10L, repeats = 3L)
  r$instantiate(task)
  tab = merge(r$instance, task$data(cols = "s")[, row_id := task$row_ids], by = "row_id")
  expect_lte(diff(range(tab[, .N, by = c("rep", "fold")]$N)), 1L)
  expect_true(all(tab[, .N, by = c("s", "rep", "fold")]$N <= 2L))
  expect_identical(tab[, uniqueN(fold), by = c("s", "rep")]$V1, rep(10L, 30L))
})

test_that("grouping", {
  r = rsmp("repeated_cv", folds = 5, repeats = 2)
  expect_grouping_works(r)
})

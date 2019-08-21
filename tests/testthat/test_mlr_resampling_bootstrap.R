context("mlr_resampling_bootstrap")

test_that("bootstrap has duplicated ids", {
  r = rsmp("bootstrap")
  expect_identical(r$duplicated_ids, TRUE)
})

test_that("stratification", {
  data = data.table(y = rep(letters[1:2], times = c(90, 10)), x1 = runif(100), x2 = rep(LETTERS[1:2], times = c(50, 50)))
  b = as_data_backend(data)
  task = TaskClassif$new("stratify_data", b, target = "y")

  r = rsmp("bootstrap", ratio = 1, repeats = 3, stratify = TRUE)
  r$instantiate(task)

  for (i in seq_len(r$iters)) {
    expect_equal(task$data(r$train_set(i))[y == "a", .N], 90)
    expect_equal(task$data(r$train_set(i))[y == "b", .N], 10)
  }
})

test_that("grouping", {
  r = rsmp("bootstrap", ratio = 1, repeats = 3)
  expect_grouping_works(r)
})

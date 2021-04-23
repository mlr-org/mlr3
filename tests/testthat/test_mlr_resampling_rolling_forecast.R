test_that("rolling forecast has no duplicated ids", {
  r = rsmp("rolling_forecast")
  expect_identical(r$duplicated_ids, FALSE)
})

test_that("construction", {
  task = tsk("airpassengers")
  r = rsmp("rolling_forecast", test_size = 3L, folds = 4L)

  r$instantiate(task)
  expect_equal(r$iters, 4L)
  expect_data_table(r$instance, ncols = 2L, nrows = 144L, key = "block")
  expect_set_equal(r$train_set(1), 1:132)
  expect_set_equal(r$test_set(1), 133:135)
  expect_set_equal(r$train_set(2), 1:135)
  expect_set_equal(r$test_set(2), 136:138)
  expect_set_equal(r$train_set(3), 1:138)
  expect_set_equal(r$test_set(3), 139:141)
  expect_set_equal(r$train_set(4), 1:141)
  expect_set_equal(r$test_set(4), 142:144)
})

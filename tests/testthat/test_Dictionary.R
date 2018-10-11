context("Dictionary")

test_that("Dictionary", {
  Foo = R6::R6Class("Foo", public = list(x=0, id=NULL, initialize = function(x) self$x = x), cloneable = TRUE)
  d = Dictionary$new()
  expect_identical(d$keys(), character(0L))

  f1 = Foo$new(1)
  f1$id = "f1"
  f2 = Foo$new(2)
  f2$id = "f2"

  d$add("f1", f1)
  expect_identical(d$keys(), "f1")
  f1c = d$get("f1")
  expect_different_address(f1, f1c)
  expect_list(d$mget("f1"), names = "unique", len = 1, types = "Foo")

  d$add("f2", f2)
  expect_set_equal(d$keys(), c("f1", "f2"))
  expect_list(d$mget(c("f1", "f2")), names = "unique", len = 2, types = "Foo")

  d$add("f2", f2)
  expect_set_equal(d$keys(), c("f1", "f2"))
  expect_list(d$mget(c("f1", "f2")), names = "unique", len = 2, types = "Foo")
})

test_that("Dictionary: lazy values", {
  expect_function(mlr_tasks$items$iris)

  t1 = mlr_tasks$get("iris")
  expect_task(t1)
  t2 = mlr_tasks$get("iris")
  expect_task(t2)
  expect_different_address(t1, t2)
})

test_that("$keys(pattern) works", {
  expect_subset(mlr_learners$keys("classif"), mlr_learners$keys(), empty.ok = FALSE)
})

test_that("Dictionaries are populated", {
  for (i in 1:2) {
    expect_dictionary(mlr_tasks, "Task", min.items = 1L)
    expect_dictionary(mlr_learners, "Learner", min.items = 1L)
    expect_dictionary(mlr_resamplings, "Resampling", min.items = 1L)
    expect_dictionary(mlr_measures, "Measure", min.items = 1L)

    expect_data_table(as.data.table(mlr_tasks), nrow = length(mlr_tasks$keys()))
    expect_data_table(as.data.table(mlr_learners), nrow = length(mlr_learners$keys()))
    expect_data_table(as.data.table(mlr_resamplings), nrow = length(mlr_resamplings$keys()))
    expect_data_table(as.data.table(mlr_measures), nrow = length(mlr_measures$keys()))
  }
})

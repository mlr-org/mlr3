context("Dictionary")

test_that("Dictionary", {
  Foo = R6::R6Class("Foo", public = list(x=0, id=NULL, initialize = function(x) self$x = x), cloneable = TRUE)
  d = Dictionary$new("Foo")
  expect_identical(d$contains, "Foo")
  expect_identical(d$ids(), character(0L))

  f1 = Foo$new(1)
  f1$id = "f1"
  f2 = Foo$new(2)
  f2$id = "f2"

  d$add(f1)
  expect_identical(d$ids(), "f1")
  f1c = d$get("f1")
  expect_different_address(f1, f1c)
  expect_list(d$mget("f1"), names = "unique", len = 1, types = "Foo")

  d$add(f2)
  expect_set_equal(d$ids(), c("f1", "f2"))
  expect_list(d$mget(c("f1", "f2")), names = "unique", len = 2, types = "Foo")

  d$add(f2)
  expect_set_equal(d$ids(), c("f1", "f2"))
  expect_list(d$mget(c("f1", "f2")), names = "unique", len = 2, types = "Foo")
})


test_that("Dictionary: lazy values", {
  # we can just test mlr_tasks here
  expect_class(mlr_tasks$items$iris, "LazyValue")
  expect_function(mlr_tasks$items$iris$getter)

  t1 = mlr_tasks$get("iris")
  expect_task(t1)
  t2 = mlr_tasks$get("iris")
  expect_task(t2)
  expect_different_address(t1, t2)
})

test_that("Dictionaries are populated", {
  for (i in 1:2) {
    if (i == 2L)
      populate_dicts()
    expect_dictionary(mlr_tasks, "Task", min.items = 1L)
    expect_dictionary(mlr_learners, "Learner", min.items = 1L)
    expect_dictionary(mlr_resamplings, "Resampling", min.items = 1L)
    expect_dictionary(mlr_measures, "Measure", min.items = 1L)
  }
})

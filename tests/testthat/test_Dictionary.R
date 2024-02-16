test_that("Dictionary: clone works", {
  t1 = tsk("iris")
  expect_task(t1)
  t2 = tsk("iris")
  expect_task(t2)
  expect_different_address(t1, t2)
})

test_that("$keys(pattern) works", {
  expect_subset(mlr_learners$keys("classif"), mlr_learners$keys(), empty.ok = FALSE)
})

test_that("dictionary to data.table conversion works with prototype arguments", {
  LearnerRegrRpart2 = R6Class("LearnerRegrRpart2",
    inherit = LearnerRegrRpart,
    public = list(
      x = NULL,
      initialize = function(x) {
        self$x = x
        super$initialize()
      }
    )
  )
  on.exit(mlr_learners$remove("regr.rpart2"))
  mlr_learners$add("regr.rpart2", LearnerRegrRpart2, .prototype_args = list(x = 123))

  expect_data_table(as.data.table(mlr_learners))
})

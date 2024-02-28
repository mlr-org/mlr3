test_that("as_learner conversion", {
  learner = lrn("classif.rpart")
  converted = as_learner(learner)
  cloned = as_learner(learner, clone = TRUE)

  expect_class(converted, "Learner")
  expect_same_address(learner, converted)
  expect_different_address(learner, cloned)

  expect_list(as_learners(learner), types = "Learner")
  expect_list(as_learners(list(learner)), types = "Learner")
})

test_that("discard_state", {
  learner = lrn("classif.rpart")$train(tsk("iris"))
  learner2 = as_learner(learner, clone = TRUE, discard_state = TRUE)
  expect_true(is.null(learner2$state))
  expect_false(is.null(learner$state))

  learner3 = lrn("classif.rpart")
  as_learner(learner3, clone = FALSE, discard_state = TRUE)
  expect_true(is.null(learner3$state))
})

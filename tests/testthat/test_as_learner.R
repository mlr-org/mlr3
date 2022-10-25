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

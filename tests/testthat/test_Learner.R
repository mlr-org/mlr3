context("Learner")

test_that("construction", {
  l = Learner$new("test-learner", task_type = "classif", predict_types = "prob")
  expect_class(l, "Learner")

})

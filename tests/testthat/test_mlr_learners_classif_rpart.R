context("mlr_learners_classif_rpart")

test_that("autotest", {
  learner = mlr_learners$get("classif.rpart")
  expect_autotest(learner)

  tasks = generate_tasks(learner)
})

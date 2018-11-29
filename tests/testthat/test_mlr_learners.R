context("mlr_learners")

test_that("mlr_learners", {
  ids = mlr_learners$keys()

  for (key in ids) {
    m = mlr_learners$get(key)
    expect_learner(m)
  }
})

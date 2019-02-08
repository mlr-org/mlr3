context("mlr_learners")

test_that("mlr_learners", {
  ids = mlr_learners$ids()

  for (key in ids) {
    m = mlr_learners$get(key)
    expect_learner(m)
  }
})

test_that("mlr_learners: sugar", {
  lrn = mlr_learners$get("classif.rpart", id = "foo", param_vals = list(cp = 0.001), predict_type = "prob")
  expect_equal(lrn$id, "foo")
  expect_equal(lrn$param_set$values$cp, 0.001)
  expect_equal(lrn$predict_type, "prob")


  lrns = mlr_learners$mget(c("classif.rpart", "classif.featureless"), predict_type = "prob")
  expect_equal(lrns[[1]]$predict_type, "prob")
  expect_equal(lrns[[2]]$predict_type, "prob")
})

context("mlr_learners")

test_that("mlr_learners", {
  expect_dictionary(mlr_learners, min_items = 1L)
  keys = mlr_learners$keys()

  for (key in keys) {
    l = mlr_learners$get(key)
    expect_learner(l)
    if (inherits(l, "TaskClassif")) {
      expect_true(startsWith(l$id, "classif."))
    }
    if (inherits(l, "TaskRegr")) {
      expect_true(startsWith(l$id, "regr."))
    }
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


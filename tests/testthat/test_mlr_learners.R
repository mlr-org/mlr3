test_that("mlr_learners", {
  expect_dictionary(mlr_learners, min_items = 1L)
  keys = mlr_learners$keys()

  for (key in keys) {
    l = lrn(key)
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
  lrn = lrn("classif.rpart", id = "foo", cp = 0.001, predict_type = "prob")
  expect_equal(lrn$id, "foo")
  expect_equal(lrn$param_set$values$cp, 0.001)
  expect_equal(lrn$predict_type, "prob")
})

test_that("extra_cols", {
  tab = as.data.table(mlr_learners, extract = function(x) list(hash = x$hash))
  expect_data_table(tab)
  expect_character(tab$hash, any.missing = FALSE)
})

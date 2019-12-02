context("Measure")

test_that("print.Measure", {
  m = mlr_measures$get("classif.ce")
  expect_output(print(Measure))
})

test_that("assert_measure", {
  task = tsk("iris")
  learner = lrn("classif.featureless")
  m = mlr_measures$get("time_train")
  expect_class(assert_measure(m), "Measure")
  expect_class(assert_measure(m, task = task), "Measure")
})

test_that("average with micro/macro", {
  task = tsk("german_credit")
  learner = lrn("classif.featureless")
  macro = msr("classif.fbeta", id = "macro")
  micro = msr("classif.fbeta", id = "micro", average = "micro")
  with_seed(123, {
    rs = rsmp("cv", folds = 5)
    rr = resample(task, learner, rs)
  })
  aggr = rr$aggregate(list(macro, micro))

  expect_true(diff(aggr) > 0)
  expect_equal(aggr[[1]], mean(map_dbl(rr$predictions(), micro$score)))
  expect_equal(aggr[[1]], mean(map_dbl(rr$predictions(), macro$score)))
  expect_equal(aggr[[2]], micro$score(rr$prediction("test")))
  expect_equal(aggr[[2]], macro$score(rr$prediction("test")))
})

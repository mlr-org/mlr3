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
  task = tsk("sonar")
  learner = lrn("classif.featureless")
  macro = msr("classif.fbeta", id = "macro")
  micro = msr("classif.fbeta", id = "micro", average = "micro")

  rr = resample(task, learner, rsmp("cv", folds = 3))
  aggr = rr$aggregate(list(macro, micro))
  mean(map_dbl(rr$predictions(), m$score))
  micro$score(rr$prediction(micro$predict_sets))
  macro$score(rr$prediction(micro$predict_sets))

  expect_equal(cmp, aggr[[1]])
})

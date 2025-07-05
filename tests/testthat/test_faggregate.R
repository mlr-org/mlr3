task = tsk("sonar")
learner = lrn("classif.rpart")
resampling = replicate(100, rsmp("holdout"))
design = benchmark_grid(task, learner, resampling)
bmr = benchmark(design)

test_that("faggregate works", {
  tab = faggregate(bmr, msr("classif.ce"))
  expect_data_table(tab, nrows = 100)
  expect_names(names(tab), identical.to = c("uhash", "classif.ce"))
  expect_numeric(tab$classif.ce, lower = 0, upper = 1)
})

test_that("faggregate returns the same as $aggregate", {
  expect_equal(faggregate(bmr, msr("classif.ce"))$classif.ce, bmr$aggregate(msr("classif.ce"))$classif.ce)

  bmr_new = c(bmr, bmr)
  expect_equal(faggregate(bmr_new, msr("classif.ce"))$classif.ce, bmr_new$aggregate(msr("classif.ce"))$classif.ce)

  bmr_new$filter(1:10)
  expect_equal(faggregate(bmr_new, msr("classif.ce"))$classif.ce, bmr_new$aggregate(msr("classif.ce"))$classif.ce)
})

test_that("faggregate throws error if measure requires task, learner, model or train set", {
  expect_error(faggregate(bmr, msr("aic")), "aggregate measure that requires")
})

test_that("faggregate works with weights", {
  task = tsk("sonar")
  task$set_col_roles("V1", "weights_measure")
  learner = lrn("classif.rpart")
  resampling = replicate(100, rsmp("holdout"))
  design = benchmark_grid(task, learner, resampling)
  bmr = benchmark(design)

  expect_equal(faggregate(bmr, msr("classif.ce"))$classif.ce, bmr$aggregate(msr("classif.ce"))$classif.ce)
})


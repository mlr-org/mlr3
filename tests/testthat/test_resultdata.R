context("ResultData")

test_that("mlr3tuning use case", {
  task = tsk("iris")
  learners = lrns(c("classif.rpart", "classif.rpart", "classif.rpart"))
  learners[[1]]$param_set$values = list(xval = 0, cp = 0.1)
  learners[[2]]$param_set$values = list(xval = 0, cp = 0.2)
  learners[[3]]$param_set$values = list(xval = 0, cp = 0.3)
  resampling = rsmp("holdout")

  bmr = benchmark(benchmark_grid(task, learners, resampling))

  rdata = bmr$data

  expect_resultdata(rdata)
  expect_data_table(rdata$data$fact, nrows = 3L)
  expect_data_table(rdata$data$tasks, nrows = 1L)
  expect_data_table(rdata$data$task_components, nrows = 1L)
  expect_data_table(rdata$data$learners, nrows = 1L)
  expect_data_table(rdata$data$learner_components, nrows = 3L)
  expect_data_table(rdata$data$resamplings, nrows = 1L)

  expect_set_equal(map_dbl(bmr$learners$learner, function(l) l$param_set$values$cp), 1:3 / 10)

  get_params = function(l) l$param_set$values$cp
  has_state = function(l) length(l$state) > 0L
  expect_set_equal(map_dbl(bmr$learners$learner, get_params), 1:3 / 10)
  expect_true(all(!map_lgl(bmr$learners$learner, has_state)))

  aggr = bmr$aggregate()
  expect_set_equal(map_dbl(map(aggr$resample_result, "learner"), get_params), 1:3 / 10)
  expect_true(all(!map_lgl(map(aggr$resample_result, "learner"), has_state)))

  scores = bmr$score()
  expect_set_equal(map_dbl(scores$learner, get_params), 1:3 / 10)
  expect_true(all(map_lgl(scores$learner, has_state)))
})



test_that("mlr3fsselect use case", {
  tasks = tsks(c("iris", "iris", "iris"))
  learner = lrn("classif.featureless")
  tasks[[1]]$col_roles$feature = names(iris)[1]
  tasks[[2]]$col_roles$feature = names(iris)[2]
  tasks[[3]]$col_roles$feature = names(iris)[3]
  resampling = rsmp("holdout")

  bmr = benchmark(benchmark_grid(tasks, learner, resampling))

  rdata = bmr$data

  expect_resultdata(rdata)
  expect_data_table(rdata$data$fact, nrows = 3L)
  expect_data_table(rdata$data$tasks, nrows = 1L)
  expect_data_table(rdata$data$task_components, nrows = 3L)
  expect_data_table(rdata$data$learners, nrows = 1L)
  expect_data_table(rdata$data$learner_components, nrows = 1L)
  expect_data_table(rdata$data$resamplings, nrows = 3L)

  expect_set_equal(map_chr(bmr$tasks$task, function(t) t$feature_names), names(iris)[1:3])

  get_feature_names = function(t) t$feature_names
  expect_set_equal(map_chr(bmr$tasks$task, get_feature_names), names(iris)[1:3])

  aggr = bmr$aggregate()
  expect_set_equal(map_chr(map(aggr$resample_result, "task"), get_feature_names), names(iris)[1:3])

  scores = bmr$score()
  expect_set_equal(map_chr(scores$task, get_feature_names), names(iris)[1:3])

  expect_set_equal(map_chr(as.data.table(bmr)$task, get_feature_names), names(iris)[1:3])
})

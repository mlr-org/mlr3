test_that("results are ordered", {
  grid = data.table(
    task = tsks(c("iris", "sonar")),
    learner = lrns(c("classif.featureless", "classif.debug")),
    resampling = rsmps("cv", folds = 3)
  )
  grid$resampling = pmap(grid, function(task, resampling, ...) resampling$clone(deep = TRUE)$instantiate(task))

  bmr = benchmark(grid, store_models = TRUE)
  rdata = get_private(bmr)$.data

  tab = rdata$as_data_table()
  expect_equal(rdata$uhashes(), rdata$data$uhashes$uhash)
  expect_equal(unique(hashes(tab$task)), hashes(grid$task))
  expect_equal(unique(hashes(tab$learner)), hashes(grid$learner))
  expect_equal(unique(hashes(tab$resampling)), hashes(grid$resampling))

  rdata$data$uhashes$uhash = rev(rdata$data$uhashes$uhash)
  tab = rdata$as_data_table()
  expect_equal(rdata$uhashes(), rdata$data$uhashes$uhash)
  expect_equal(unique(hashes(tab$task)), rev(hashes(grid$task)))
  expect_equal(unique(hashes(tab$learner)), rev(hashes(grid$learner)))
  expect_equal(unique(hashes(tab$resampling)), rev(hashes(grid$resampling)))

  rr = resample(tsk("pima"), lrn("classif.rpart"), rsmp("holdout"))
  rdata$combine(private(rr)$.data)
  expect_resultdata(rdata)
  expect_equal(rdata$uhashes()[3], rr$uhash)

  # remove rr in the middle
  uhashes = rdata$uhashes()
  rdata$data$fact = rdata$data$fact[!list(uhashes[2])]
  rdata$sweep()
  expect_resultdata(rdata, TRUE)

  expect_equal(rdata$uhashes(), uhashes[c(1, 3)])

  # test discard
  expect_true(!every(map(rdata$data$fact$learner_state, "model"), is.null))
  expect_true(!some(map(rdata$data$tasks$task, "backend"), is.null))

  rdata$discard(models = TRUE)
  expect_true(every(map(rdata$data$fact$learner_state, "model"), is.null))

  rdata$discard(backends = TRUE)
  expect_true(every(map(rdata$data$tasks$task, "backend"), is.null))
})

test_that("mlr3tuning use case", {
  task = tsk("iris")
  learners = lrns(c("classif.rpart", "classif.rpart", "classif.rpart"))
  learners[[1]]$param_set$values = list(xval = 0, cp = 0.1)
  learners[[2]]$param_set$values = list(xval = 0, cp = 0.2)
  learners[[3]]$param_set$values = list(xval = 0, cp = 0.3)
  resampling = rsmp("holdout")

  bmr = benchmark(benchmark_grid(task, learners, resampling))

  rdata = get_private(bmr)$.data

  expect_resultdata(rdata)
  expect_data_table(rdata$data$fact, nrows = 3L)
  expect_data_table(rdata$data$tasks, nrows = 1L)
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

  learner_states = rdata$learner_states()
  expect_list(learner_states, any.missing = FALSE, len = 3)
  expect_set_equal(map_dbl(learner_states, function(l) l$param_vals$cp), 1:3 / 10)
})

test_that("predict set selection", {
  task = tsk("mtcars")
  learner = lrn("regr.rpart", predict_sets = c("train", "test"))
  resampling = rsmp("holdout")
  rr = resample(task, learner, resampling)

  p1 = rr$predictions("train")[[1]]
  p2 = rr$predictions("test")[[1]]
  expect_prediction(p1)
  expect_prediction(p2)
  expect_disjunct(p1$row_ids, p2$row_ids)

  p1 = rr$prediction("train")
  p2 = rr$prediction("test")
  expect_prediction(p1)
  expect_prediction(p2)
  expect_disjunct(p1$row_ids, p2$row_ids)
})

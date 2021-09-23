task = tsk("penguins")
learner = lrn("classif.rpart")
rr = resample(task, learner, rsmp("cv", folds = 3), store_models = TRUE)

test_that("similarity measures", {
  measures = mlr_measures$keys("^similarity")

  for (m in msrs(measures)) {
    expect_number(m$aggregate(rr))
    expect_true(is.na(m$score(rr$prediction(), learner = rr$learners[[1]])))
    expect_numeric(rr$aggregate(msrs(c("classif.acc", m$id))), len = 2, any.missing = FALSE)
    expect_true(allMissing(rr$score(m)[[m$id]]))
  }
})

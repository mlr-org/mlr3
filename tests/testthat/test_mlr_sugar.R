test_that("singular sugar functions", {
  expect_task(tsk("iris"))
  expect_learner(lrn("classif.featureless"))
  expect_resampling(rsmp("cv"))
  expect_measure(msr("classif.ce"))
})

test_that("plural sugar functions", {
  expect_list(tsks("iris"), "Task", len = 1L)
  expect_list(lrns("classif.featureless"), "Learner", len = 1L)
  expect_list(rsmps("cv"), "Resampling", len = 1L)
  expect_list(msrs("classif.ce"), "Measure", len = 1L)
})

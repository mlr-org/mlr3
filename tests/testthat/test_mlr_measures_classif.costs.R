test_that("binary task", {
  task = tsk("sonar")
  costs = matrix(c(0, 5, 10, 0), nrow = 2)
  rownames(costs) = colnames(costs) = task$class_names

  m1 = mlr_measures$get("classif.costs")
  m1$costs = costs
  m1$id = "m1"
  m2 = msr("classif.costs", costs = -1 * costs)
  m2$id = "m2"
  costs[] = 0
  m3 = msr("classif.costs", costs = costs)
  m3$id = "m3"
  measures = list(m1, m2, m3)

  lrn = lrn("classif.featureless")
  perf = lrn$train(task)$predict(task)$score(measures)
  expect_equal(perf[[1]], -perf[[2]])
  expect_equal(perf[[3]], 0)
})

test_that("multiclass", {
  task = tsk("iris")
  costs = 1 - diag(3)
  rownames(costs) = colnames(costs) = task$class_names

  m = msr("classif.costs", costs = costs, normalize = FALSE)
  measures = list(m, mlr_measures$get("classif.ce"))

  lrn = lrn("classif.featureless")
  p = lrn$train(task)$predict(task)
  perf = p$score(measures, task = task)
  expect_equal(perf[["classif.costs"]], perf[["classif.ce"]] * task$nrow)
})

test_that("multiclass / level reordering", {
  task = tsk("iris")
  costs = matrix(runif(9), nrow = 3)
  diag(costs) = 0
  rownames(costs) = colnames(costs) = task$class_names

  m1 = msr("classif.costs", id = "c1", costs = costs, normalize = FALSE)
  costs = costs[c(2, 1, 3), c(3, 2, 1)]
  m2 = msr("classif.costs", id = "c2", costs = costs, normalize = FALSE)
  measures = list(m1, m2)

  lrn = lrn("classif.featureless")
  perf = lrn$train(task)$predict(task)$score(measures, task = task)
  expect_equal(unname(perf[1]), unname(perf[2]))
})

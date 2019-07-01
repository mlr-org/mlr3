context("mlr_measures_classif.costs")

test_that("binary task", {
  task = mlr_tasks$get("sonar")
  costs = matrix(c(0, 5, 10, 0), nrow = 2)
  rownames(costs) = colnames(costs) = task$class_names

  m1 = mlr_measures$get("classif.costs", costs = costs)
  m1$id = "m1"
  m2 = mlr_measures$get("classif.costs", costs = -1 * costs)
  m2$id = "m2"
  costs[] = 0
  m3 = mlr_measures$get("classif.costs", costs = costs)
  m3$id = "m3"
  measures = list(m1, m2, m3)

  lrn = mlr_learners$get("classif.featureless")
  perf = lrn$train(task)$predict(task)$score(measures, task = task)
  expect_equal(perf[[1]], -perf[[2]])
  expect_equal(perf[[3]], 0)
})

test_that("multiclass", {
  task = mlr_tasks$get("iris")
  costs = 1 - diag(3)
  rownames(costs) = colnames(costs) = task$class_names

  m = mlr_measures$get("classif.costs", costs = costs, normalize = FALSE)
  measures = list(m, mlr_measures$get("classif.ce"))

  lrn = mlr_learners$get("classif.featureless")
  p = lrn$train(task)$predict(task)
  perf = p$score(measures, task = task)
  expect_equal(perf[["classif.costs"]], perf[["classif.ce"]] * task$nrow)
})

test_that("multiclass / level reordering", {
  task = mlr_tasks$get("iris")
  costs = matrix(runif(9), nrow = 3)
  diag(costs) = 0
  rownames(costs) = colnames(costs) = task$class_names

  m1 = mlr_measures$get("classif.costs", costs = costs, normalize = FALSE)
  costs = costs[c(2, 1, 3), c(3, 2, 1)]
  m2 = mlr_measures$get("classif.costs", costs = costs, normalize = FALSE)
  measures = list(m1, m2)

  lrn = mlr_learners$get("classif.featureless")
  perf = lrn$train(task)$predict(task)$score(measures, task = task)
  expect_equal(perf[1], perf[2])
})

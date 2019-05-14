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
  task$measures = list(m1, m2, m3)

  e = Experiment$new(task, mlr_learners$get("classif.featureless"))$train()$predict()$score()
  perf = e$performance

  expect_equal(perf[[1]], -perf[[2]])
  expect_equal(perf[[3]], 0)
})

test_that("multiclass", {
  task = mlr_tasks$get("iris")
  costs = 1 - diag(3)
  rownames(costs) = colnames(costs) = task$class_names

  m = mlr_measures$get("classif.costs", costs = costs, normalize = FALSE)
  task$measures = list(m, mlr_measures$get("classif.ce"))

  e = Experiment$new(task, "classif.featureless")$train()$predict()$score()
  perf = e$performance

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
  task$measures = list(m1, m2)

  e = Experiment$new(task, mlr_learners$get("classif.featureless"))$train()$predict()$score()
  e$prediction
  perf = e$performance

  expect_equal(perf[1], perf[2])
})

context("benchmark")

test_that("Basic benchmarking", {
  tasks = mlr_tasks$mget(c("iris", "sonar"))
  learners = lapply(c("classif.dummy", "classif.rpart"), mlr_learners$get)
  resamplings = lapply("cv", mlr_resamplings$get)
  bmr = benchmark(tasks, learners, resamplings)

  expect_is(bmr, "BenchmarkResult")
  expect_names(names(bmr$data), permutation.of = reflections$experiment_slots$name)

  expect_experiment(bmr$experiment(1))

  expect_data_table(bmr$performance, nrow = 40)
  expect_names(names(bmr$performance), permutation.of = c("task", "learner", names(tasks[[1L]]$measures)))
})

test_that("default_values", {
  learner = lrn("classif.debug")
  search_space = ps(iter = p_int(1, 10))
  task = tsk("pima")

  values = default_values(learner, search_space, task)
  expect_names(names(values), identical.to = "iter")
})

test_that("default_values works with empty search space", {
  learner = lrn("classif.debug")
  expect_list(default_values(learner, ps(), task), len = 0)
})

testthat("default_values on rpart", {
  learner = lrn("classif.rpart")
  search_space = ps(
    minsplit = p_int(2, 128, logscale = TRUE),
    minbucket = p_int(1, 64, logscale = TRUE),
    cp = p_dbl(1e-04, 1e-1, logscale = TRUE)
  )
  task = tsk("pima")

  values = default_values(learner, search_space, task)
  expect_names(names(values), permutation.of = c("minsplit", "minbucket", "cp"))
})

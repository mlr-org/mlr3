context("TaskClassif")

test_that("Basic ops on iris task", {
  task = mlr_tasks$get("iris")
  expect_task(task)
  expect_task_supervised(task)
  expect_task_classif(task)
  expect_equal(task$target_names, "Species")
  expect_set_equal(task$classes, levels(iris$Species))
  expect_identical(task$nclasses, nlevels(iris$Species))

  f = task$formula
  expect_class(f, "formula")
  expect_set_equal(attr(terms(f), "term.labels"), setdiff(names(iris), "Species"))
})

test_that("$classes and $nclasses only consider active rows", {
  task = mlr_tasks$get("iris")
  task$rows[1:100, role := "ignore"]

  expect_identical(task$classes, "virginica")
  expect_identical(task$nclasses, 1L)
})

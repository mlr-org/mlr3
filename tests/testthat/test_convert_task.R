test_that("convert_task - Regr -> Regr", {
  task = tsk("california_housing")
  result = convert_task(task, target = "households", drop_original_target = TRUE)

  expect_class(result, "TaskRegr")
  expect_task(result)
  expect_true(result$col_roles$target == "households")
  expect_true(all(result$feature_names != "households"))
  expect_true(all(result$feature_names != "median_house_value"))
  expect_true(all(unlist(imap(result$row_roles,
    .f = function(z, x) {
      all(result$row_roles[[x]] == task$row_roles[[x]])
    }
  ))))
  expect_true(
    every(c("weights_learner", "groups", "strata", "nrow"), function(x) {
      all(result[[x]] == task[[x]])
    }))
})

test_that("convert_task - Regr -> Classif", {
  task = tsk("california_housing")
  result = convert_task(task, target = "ocean_proximity", new_type = "classif", drop_original_target = TRUE)

  expect_class(result, "TaskClassif")
  expect_task(result)
  expect_true(result$col_roles$target == "ocean_proximity")
  expect_true(all(result$feature_names != "ocean_proximity"))
  expect_true(all(result$feature_names != "median_house_value"))
  expect_true(all(unlist(imap(result$row_roles,
    .f = function(z, x) {
      all(result$row_roles[[x]] == task$row_roles[[x]])
    }
  ))))
  expect_true(
    every(c("weights_learner", "groups", "strata", "nrow"), function(x) {
      all(result[[x]] == task[[x]])
    }))
})

test_that("convert_task - Classif -> Regr", {
  task = tsk("iris")
  result = convert_task(task, target = "Sepal.Width", new_type = "regr", drop_original_target = TRUE)

  expect_class(result, "TaskRegr")
  expect_task(result)
  expect_true(result$col_roles$target == "Sepal.Width")
  expect_true(all(result$feature_names != "Sepal.Width"))
  expect_true(all(result$feature_names != "Species"))
  expect_true(all(unlist(imap(result$row_roles,
    .f = function(z, x) {
      all(result$row_roles[[x]] == task$row_roles[[x]])
    }
  ))))
  expect_true(
    every(c("weights_learner", "groups", "strata", "nrow"), function(x) {
      all(result[[x]] == task[[x]])
    }))
})

test_that("convert_task - same target", {
  task = tsk("california_housing")
  task$col_roles$feature = setdiff(task$col_roles$feature, "latitue")

  results = list(
    convert_task(task, target = "median_house_value", new_type = "regr", drop_original_target = TRUE),
    convert_task(task, target = "median_house_value", new_type = "regr", drop_original_target = FALSE)
  )

  for (result in results) {
    expect_class(result, "TaskRegr")
    expect_task(result)
    expect_true(result$col_roles$target == "median_house_value")
    expect_true(all(unlist(imap(result$row_roles,
      .f = function(z, x) {
        all(result$row_roles[[x]] == task$row_roles[[x]])
      }
    ))))
    expect_true(
      every(
        c("weights_learner", "groups", "strata", "nrow", "ncol", "feature_names", "target_names", "task_type"),
        function(x) {
          all(result[[x]] == task[[x]])
        }
    ))
  }
})

test_that("convert task - general checks", {
  btask = tsk("california_housing")
  itask = tsk("iris")

  # target does not exist
  expect_error(convert_task(btask, target = "median_house_value2"))

  # target class does not match
  expect_error(convert_task(btask, target = "latitude", new_type = "classif"))
  expect_error(convert_task(itask, target = "Sepal.Width", new_type = "classif"))
})

test_that("convert_task reconstructs task", {
  task = tsk("iris")
  tsk = convert_task(task)
  tsk$man = "mlr3::mlr_tasks_iris"
  # TODO: re-enable after task$weights has been removed
  # expect_equal(task, tsk, ignore_attr = TRUE)

  task2 = task$filter(1:100)
  tsk2 = convert_task(task2)
  # TODO: re-enable after task$weights has been removed
  # expect_equal(task2$nrow, tsk2$nrow)
  # expect_equal(task2$ncol, tsk2$ncol)
  expect_true("twoclass" %chin% tsk2$properties)

  task3 = task2
  task3$row_roles$use = 1:150
  tsk3 = convert_task(task3)
  tsk3$man = "mlr3::mlr_tasks_iris"
  # TODO: re-enable after task$weights has been removed
  # expect_equal(task3$nrow, tsk3$nrow)
  # expect_equal(task3$ncol, tsk3$ncol)
  expect_true("multiclass" %chin% tsk3$properties)
  # TODO: re-enable after task$weights has been removed
  # expect_equal(task, tsk3, ignore_attr = TRUE)
})

test_that("extra args survive the roundtrip", {
  mytask = tsk("sonar")
  expect_equal(mytask$extra_args, list(positive = "M"))

  mytask = convert_task(mytask, target = "V1", new_type = "regr")
  expect_equal(mytask$extra_args, list(positive = "M"))

  mytask = convert_task(mytask, target = "Class", new_type = "classif")
  expect_equal(mytask$extra_args, list(positive = "M"))
  expect_equal(mytask$positive, "M")

  mytask$positive = mytask$negative
  expect_equal(mytask$extra_args, list(positive = "R"))

  mytask = convert_task(mytask, target = "V1", new_type = "regr")
  expect_equal(mytask$extra_args, list(positive = "R"))

  mytask = convert_task(mytask, target = "Class", new_type = "classif")
  expect_equal(mytask$extra_args, list(positive = "R"))
  expect_equal(mytask$positive, "R")
})

test_that("data.frame converters", {
  data("mtcars", package = "datasets")
  task = as_task_regr(mtcars, "mpg")
  expect_task_regr(task)
  expect_equal(task$id, "mtcars")

  data("iris", package = "datasets")
  task = as_task_classif(iris, "Species")
  expect_task_classif(task)
  expect_equal(task$id, "iris")

  task = as_task_unsupervised(mtcars)
  expect_task_unsupervised(task)
})

test_that("formula converters", {
  data("mtcars", package = "datasets")
  task = as_task_regr(mpg ~ am + carb, mtcars)
  expect_task_regr(task)
  expect_equal(task$id, "mtcars")
  expect_set_equal(task$feature_names, c("am", "carb"))
  expect_equal(task$target_names, "mpg")

  my_mtcars = mtcars
  my_mtcars$hp[5] = NA
  task = as_task_regr(mpg ~ ., my_mtcars)
  expect_task_regr(task)
  expect_equal(task$id, "my_mtcars")
  expect_set_equal(task$feature_names, setdiff(names(my_mtcars), "mpg"))
  expect_equal(task$target_names, "mpg")
  expect_equal(task$nrow, nrow(my_mtcars))

  expect_error(as_task_regr(mpg ~ am + x, mtcars), "Assertion on 'formula' failed")
  expect_error(as_task_regr(~ am + carb, mtcars), "is missing a response")

  data("iris", package = "datasets")
  task = as_task_classif(Species ~ Sepal.Length + Sepal.Width, iris)
  expect_task_classif(task)
  expect_equal(task$id, "iris")
  expect_set_equal(task$feature_names, c("Sepal.Length", "Sepal.Width"))
  expect_equal(task$target_names, "Species")

  data("penguins", package = "palmerpenguins")
  task = as_task_classif(species ~ ., penguins)
  expect_task_classif(task)
  expect_equal(task$id, "penguins")
  expect_set_equal(task$feature_names, setdiff(names(penguins), "species"))
  expect_equal(task$target_names, "species")
  expect_equal(task$nrow, nrow(penguins))

  expect_error(as_task_classif(Species ~ Sepal.Length + x, iris), "Assertion on 'formula' failed")
  expect_error(as_task_classif(~ Sepal.Length + Sepal.Width, iris), "is missing a response")
})

test_that("matrix converters", {
  X = matrix(1:9, nrow = 3)
  colnames(X) = letters[1:3]
  expect_task(as_task_regr(X, target = "a"))
  expect_task(as_task_classif(X, target = "a"))
})

test_that("Matrix converters", {
  requireNamespace("Matrix", quietly = TRUE)
  X = Matrix::Matrix(1:9, nrow = 3)
  force(X)
  colnames(X) = letters[1:3]

  task = as_task_regr(X, target = "a")
  expect_task(task)

  task = as_task_classif(X, target = "a", id = "foo")
  expect_task(task)
})

test_that("convert_task - Regr -> Regr with weights", {
  task = cars_weights_learner
  result = convert_task(task, target = "speed", drop_original_target = FALSE)

  expect_equal(result$weights_learner, task$weights_learner)

  task = cars_weights_measure
  result = convert_task(task, target = "speed", drop_original_target = FALSE)

  expect_equal(result$weights_measure, task$weights_measure)

})

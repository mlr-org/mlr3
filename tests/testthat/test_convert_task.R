test_that("convert_task - Regr -> Regr", {
  task = mlr_tasks$get("boston_housing")
  result = convert_task(task, target = "age", drop_original_target = TRUE)

  expect_class(result, "TaskRegr")
  expect_task(result)
  expect_true(result$col_roles$target == "age")
  expect_true(all(result$feature_names != "age"))
  expect_true(all(result$feature_names != "medv"))
  expect_true(all(unlist(imap(result$row_roles,
          .f = function(z, x) {all(result$row_roles[[x]] == task$row_roles[[x]])}))
      ))
  expect_true(
    all(map_lgl(c("weights", "groups", "strata", "nrow"), function(x) {
        all(result[[x]] == task[[x]])
  })))
})

test_that("convert_task - Regr -> Classif", {
  task = mlr_tasks$get("boston_housing")
  result = convert_task(task, target = "chas", new_type = "classif", drop_original_target = TRUE)

  expect_class(result, "TaskClassif")
  expect_task(result)
  expect_true(result$col_roles$target == "chas")
  expect_true(all(result$feature_names != "chas"))
  expect_true(all(result$feature_names != "medv"))
  expect_true(all(unlist(imap(result$row_roles,
    .f = function(z, x) {all(result$row_roles[[x]] == task$row_roles[[x]])}))
  ))
  expect_true(
    all(map_lgl(c("weights", "groups", "strata", "nrow"), function(x) {
      all(result[[x]] == task[[x]])
  })))
})

test_that("convert_task - Classif -> Regr", {
  task = mlr_tasks$get("iris")
  result = convert_task(task, target = "Sepal.Width", new_type = "regr", drop_original_target = TRUE)

  expect_class(result, "TaskRegr")
  expect_task(result)
  expect_true(result$col_roles$target == "Sepal.Width")
  expect_true(all(result$feature_names != "Sepal.Width"))
  expect_true(all(result$feature_names != "Species"))
  expect_true(all(unlist(imap(result$row_roles,
    .f = function(z, x) {all(result$row_roles[[x]] == task$row_roles[[x]])}))
  ))
  expect_true(
    all(map_lgl(c("weights", "groups", "strata", "nrow"), function(x) {
      all(result[[x]] == task[[x]])
  })))
})

test_that("convert_task - same target", {
  task = tsk("boston_housing")
  task$col_roles$feature = setdiff(task$col_roles$feature, "lat")

  results = list(
    convert_task(task, target = "medv", new_type = "regr", drop_original_target = TRUE),
    convert_task(task, target = "medv", new_type = "regr", drop_original_target = FALSE)
  )

  for (result in results) {
    expect_class(result, "TaskRegr")
    expect_task(result)
    expect_true(result$col_roles$target == "medv")
    expect_true(all(unlist(imap(result$row_roles,
      .f = function(z, x) {all(result$row_roles[[x]] == task$row_roles[[x]])}))
    ))
    expect_true(
      all(map_lgl(
        c("weights", "groups", "strata", "nrow", "ncol", "feature_names", "target_names",
          "task_type"),
        function(x) {all(result[[x]] == task[[x]])}
    )))
  }
})

test_that("convert task - general checks", {
  btask = mlr_tasks$get("boston_housing")
  itask = mlr_tasks$get("iris")

  # target does not exist
  expect_error(convert_task(btask, target = "medv2"))

  # target class does not match
  expect_error(convert_task(btask, target = "medv", new_type = "classif"))
  expect_error(convert_task(itask, target = "Sepal.Length", new_type = "classif"))
})

test_that("convert_task reconstructs task", {
  task = mlr_tasks$get("iris")
  tsk = convert_task(task)
  tsk$man = "mlr3::mlr_tasks_iris"
  expect_equal(task, tsk, ignore_attr = TRUE)

  task2 = task$filter(1:100)
  tsk2 = convert_task(task2)
  expect_equal(task2$nrow, tsk2$nrow)
  expect_equal(task2$ncol, tsk2$ncol)
  expect_true("twoclass" %in% tsk2$properties)

  task3 = task2
  task3$row_roles$use = 1:150
  tsk3 = convert_task(task3)
  tsk3$man = "mlr3::mlr_tasks_iris"
  expect_equal(task3$nrow, tsk3$nrow)
  expect_equal(task3$ncol, tsk3$ncol)
  expect_true("multiclass" %in% tsk3$properties)
  expect_equal(task, tsk3, ignore_attr = TRUE)
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
})

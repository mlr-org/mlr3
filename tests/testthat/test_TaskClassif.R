context("TaskClassif")

test_that("Basic ops on iris task", {
  task = tsk("iris")
  expect_task(task)
  expect_task_supervised(task)
  expect_task_classif(task)
  expect_equal(task$target_names, "Species")
  expect_set_equal(task$class_names, levels(iris$Species))

  f = task$formula()
  expect_class(f, "formula")
  # expect_set_equal(attr(terms(f), "term.labels"), setdiff(names(iris), "Species"))
})

test_that("$class_names consider also inactive rows", {
  task = tsk("iris")
  task$set_row_role(1:100, character())

  expect_set_equal(task$class_names, levels(iris$Species))
})

test_that("Factor levels are preserved in prediction", {
  task = tsk("iris")
  learner = lrn("classif.featureless")
  learner$predict_type = "prob"
  learner$train(task, 1:100)

  pred = as.data.table(learner$predict(task, 1:10))
  expect_factor(pred$truth, levels = levels(iris$Species), any.missing = FALSE)
  expect_factor(pred$response, levels = levels(iris$Species), any.missing = FALSE)
  expect_equal(levels(pred$truth), levels(pred$response))
  expect_numeric(pred$prob.virginica, lower = 0, upper = 0, any.missing = FALSE)

  pred = as.data.table(learner$predict(task, 101:150))
  expect_factor(pred$truth, levels = levels(iris$Species), any.missing = FALSE)
  expect_factor(pred$response, levels = levels(iris$Species), any.missing = FALSE)
  expect_equal(levels(pred$truth), levels(pred$response))
  expect_numeric(pred$prob.virginica, lower = 0, upper = 0, any.missing = FALSE)
})

test_that("Target is character/factor", {
  b = as_data_backend(iris)
  expect_error(TaskClassif$new("iris", backend = b, target = "Sepal.Length"), "Target column")
})

test_that("0 feature task", {
  b = as_data_backend(iris[, 5L, drop = FALSE])
  task = TaskClassif$new(id = "zero_feat_task", b, target = "Species")
  expect_output(print(task))
  b = task$backend
  expect_backend(b)
  expect_task(task)
  expect_task_supervised(task)
  expect_task_classif(task)
  expect_data_table(task$data(), ncols = 1L)

  lrn = lrn("classif.featureless")
  p = lrn$train(task)$predict(task)
  expect_prediction(p)
  # expect_number(e$performance, lower = 0.6, upper = 0.7)
})

test_that("Positive class always comes first", {
  sonar = load_dataset("Sonar", package = "mlbench")
  tmp = list(c("M", "R"), c("R", "M"))
  lrn = lrn("classif.featureless", predict_type = "prob", method = "sample")

  for (lvls in tmp) {
    task = TaskClassif$new("sonar", backend = sonar, target = "Class", positive = lvls[[1]])
    expect_equal(task$positive, lvls[1])
    expect_equal(task$negative, lvls[2])
    expect_equal(task$class_names, lvls)
    expect_equal(levels(task$truth()), lvls)
    preds = lrn$train(task)$predict(task)
    expect_equal(levels(preds$truth), lvls)
    expect_equal(levels(preds$response), lvls)
    expect_set_equal(colnames(preds$prob), lvls)
  }
})

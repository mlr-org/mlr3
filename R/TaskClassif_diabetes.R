#' @title Synthetic Diabetes Classification Task
#'
#' @name mlr_tasks_diabetes
#' @format [R6::R6Class] inheriting from [TaskClassif].
#' @include mlr_tasks.R
#'
#' @description
#' A synthetic binary classification task that mimics the structure of the former `pima` task.
#' It has the same eight numeric features and a `diabetes` target with the positive class set to `"pos"`.
#' Some feature columns contain missing values, which makes the task useful for preprocessing examples and tests.
#' The data is fully synthetic and contains no real patient data.
#'
#' @templateVar id diabetes
#' @template task
#'
#' @source
#' The data set is generated deterministically by the script in `system.file("extdata", "diabetes.R", package = "mlr3")`.
#'
#' @template seealso_task
NULL

load_task_diabetes = function(id = "diabetes") {
  b = as_data_backend(readRDS(system.file("extdata", "diabetes.rds", package = "mlr3")))
  task = TaskClassif$new(id, b, target = "diabetes", positive = "pos", label = "Synthetic Diabetes")
  b$hash = task$man = "mlr3::mlr_tasks_diabetes"
  task
}

#' @include mlr_tasks.R
mlr_tasks$add("diabetes", load_task_diabetes)

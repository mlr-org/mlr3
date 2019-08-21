#' @title Boston Housing Regression Task
#'
#' @name mlr_tasks_boston_housing
#' @format [R6::R6Class] inheriting from [TaskRegr].
#' @include mlr_tasks.R
#'
#' @section Construction:
#' ```
#' mlr_tasks$get("boston_housing")
#' tsk("boston_housing")
#' ```
#'
#' @description
#' A regression task for the [mlbench::BostonHousing2] data set.
#'
#' @template seealso_task
NULL

load_task_boston_housing = function(id = "boston_housing") {
  b = as_data_backend(load_dataset("BostonHousing2", "mlbench"))
  b$hash = "_mlr3_tasks_boston_housing_"
  TaskRegr$new(id, b, target = "medv")
}

#' @include mlr_tasks.R
mlr_tasks$add("boston_housing", load_task_boston_housing)

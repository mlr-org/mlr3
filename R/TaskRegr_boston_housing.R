#' @title Boston Housing Regression Task
#'
#' @name mlr_tasks_boston_housing
#' @format [R6::R6Class] inheriting from [TaskRegr].
#' @include mlr_tasks.R
#'
#'
#' @description
#' A regression task for the [mlbench::BostonHousing2] data set.
#' This is the corrected data using the corrected median value (`cmedv`) as target.
#' The uncorrected target (`medv`) is removed from the data.
#'
#' @section Construction:
#' ```
#' mlr_tasks$get("boston_housing")
#' tsk("boston_housing")
#' ```
#'
#' @section Meta Information:
#' `r rd_info(tsk("boston_housing"))`
#'
#' @template seealso_task
NULL

load_task_boston_housing = function(id = "boston_housing") {
  b = as_data_backend(remove_named(load_dataset("BostonHousing2", "mlbench"), "medv"))
  task = TaskRegr$new(id, b, target = "cmedv", label = "Boston Housing Prices")
  b$hash = task$man = "mlr3::mlr_tasks_boston_housing"
  task
}

#' @include mlr_tasks.R
mlr_tasks$add("boston_housing", load_task_boston_housing)

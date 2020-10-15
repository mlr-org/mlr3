#' @title Air Passengers Regression Task
#'
#' @name mlr_tasks_airpassengers
#' @format [R6::R6Class] inheriting from [TaskRegr].
#' @include mlr_tasks.R
#'
#' @description
#' A toy regression task for the [datasets::AirPassengers] data set.
#' The task represents a monthly time series and is ordered by
#' its only feature `date`.
#'
#' @section Construction:
#' ```
#' mlr_tasks$get("airpassengers")
#' tsk("airpassengers")
#' ```
#'
#' @section Meta Information:
#' `r rd_info(tsk("airpassengers"))`
#'
#' @template seealso_task
NULL

load_task_airpassengers = function(id = "airpassengers") {
  ts = load_dataset("AirPassengers", "datasets")
  dates = unclass(stats::time(ts))
  dates = as.Date(paste((time_x + 0.001) %/% 1, stats::cycle(ts), 1, sep = "-"))

  tab = cbind(data.frame(date = dates), passengers = as.matrix(ts))
  b = as_data_backend(tab)
  task = TaskRegr$new(id, b, target = "passengers")
  task$col_roles$order = "date"
  b$hash = task$man = "mlr3::mlr_tasks_airpassengers"
  task
}

#' @include mlr_tasks.R
mlr_tasks$add("airpassengers", load_task_airpassengers)

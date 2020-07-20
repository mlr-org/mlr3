#' @title Sonar Classification Task
#'
#' @name mlr_tasks_sonar
#' @format [R6::R6Class] inheriting from [TaskClassif].
#' @include mlr_tasks.R
#'
#' @description
#' A classification task for the [mlbench::Sonar] data set.
#' Positive class is set to "M" (Mine).
#'
#' @section Construction:
#' ```
#' mlr_tasks$get("sonar")
#' tsk("sonar")
#' ```
#'
#' @section Meta Information:
#' `r rd_info(tsk("sonar"))`
#'
#' @template seealso_task
NULL

load_task_sonar = function(id = "sonar") {
  b = as_data_backend(load_dataset("Sonar", "mlbench"))
  task = TaskClassif$new(id, b, target = "Class", positive = "M")
  b$hash = task$man = "mlr3::mlr_tasks_sonar"
  task
}

#' @include mlr_tasks.R
mlr_tasks$add("sonar", load_task_sonar)

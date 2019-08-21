#' @title Zoo Classification Task
#'
#' @name mlr_tasks_zoo
#' @format [R6::R6Class] inheriting from [TaskClassif].
#' @include mlr_tasks.R
#'
#' @section Construction:
#' ```
#' mlr_tasks$get("zoo")
#' tsk("zoo")
#' ```
#'
#' @description
#' A classification task for the [mlbench::Zoo] data set.
NULL

load_task_zoo = function(id = "zoo") {
  b = as_data_backend(load_dataset("Zoo", "mlbench", keep_rownames = TRUE))
  b$hash = "_mlr3_tasks_zoo_"
  TaskClassif$new(id, b, target = "type")
}

#' @include mlr_tasks.R
mlr_tasks$add("zoo", load_task_zoo)

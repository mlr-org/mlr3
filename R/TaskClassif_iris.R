#' @title Iris Classification Task
#'
#' @name mlr_tasks_iris
#' @format [R6::R6Class] inheriting from [TaskClassif].
#' @include mlr_tasks.R
#'
#' @section Usage:
#' ```
#' mlr_tasks$get("iris")
#' tsk("iris")
#' ```
#'
#' @description
#' A classification task for the popular [datasets::iris] data set.
NULL

load_task_iris = function(id = "iris") {
  b = as_data_backend(load_dataset("iris", "datasets"))
  b$hash = "_mlr3_tasks_iris_"
  TaskClassif$new(id, b, target = "Species")
}

#' @include mlr_tasks.R
mlr_tasks$add("iris", load_task_iris)

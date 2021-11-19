#' @title Iris Classification Task
#'
#' @name mlr_tasks_iris
#' @format [R6::R6Class] inheriting from [TaskClassif].
#' @include mlr_tasks.R
#'
#' @description
#' A classification task for the popular [datasets::iris] data set.
#'
#' @templateVar id iris
#' @template task
#'
#' @source
#' \url{https://en.wikipedia.org/wiki/Iris_flower_data_set}
#'
#' `r format_bib("anderson_1936")`
#'
#' @template seealso_task
NULL

load_task_iris = function(id = "iris") {
  b = as_data_backend(load_dataset("iris", "datasets"))
  task = TaskClassif$new(id, b, target = "Species")
  b$hash = task$man = "mlr3::mlr_tasks_iris"
  task
}

#' @include mlr_tasks.R
mlr_tasks$add("iris", load_task_iris)

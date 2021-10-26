#' @title Pima Indian Diabetes Classification Task
#'
#' @name mlr_tasks_pima
#' @format [R6::R6Class] inheriting from [TaskClassif].
#' @include mlr_tasks.R
#'
#' @description
#' A classification task for the [mlbench::PimaIndiansDiabetes2] data set.
#' Positive class is set to `"pos"`.
#'
#' @templateVar id pima
#' @template task
#'
#' @template seealso_task
NULL

load_task_pima = function(id = "pima") {
  b = as_data_backend(load_dataset("PimaIndiansDiabetes2", "mlbench"))
  task = TaskClassif$new(id, b, target = "diabetes", positive = "pos")
  b$hash = task$man = "mlr3::mlr_tasks_pima"
  task
}

#' @include mlr_tasks.R
mlr_tasks$add("pima", load_task_pima)

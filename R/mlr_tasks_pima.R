#' @title Pima Indian Diabetes Classification Task
#'
#' @name mlr_tasks_pima
#' @format [R6::R6Class] inheriting from [TaskClassif].
#' @include mlr_tasks.R
#'
#' @description
#' A classification task for the [mlbench::PimaIndiansDiabetes2] data set.
#' Positive class is set to "pos".
mlr_tasks$add("pima", function(id = "pima") {
  b = as_data_backend(load_dataset("PimaIndiansDiabetes2", "mlbench"))
  b$hash = "_mlr3_tasks_pima_"
  TaskClassif$new(id, b, target = "diabetes", positive = "pos")
})


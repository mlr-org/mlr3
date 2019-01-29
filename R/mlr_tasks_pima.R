#' @title Pima Indian Diabetes Classification Task
#' @name mlr_tasks_pima
#' @description
#' A classification task for the [mlbench::PimaIndiansDiabetes2] data set.
#' Positive class is set to "pos".
#' @include mlr_tasks.R
mlr_tasks$add("pima", function() {
  b = as_data_backend(load_dataset("PimaIndiansDiabetes2", "mlbench"))
  b$hash = "_mlr_tasks_pima_"
  TaskClassif$new("pima_indians", b, target = "diabetes", positive = "pos")
})


#' @title Sonar Classification Task
#' @name mlr_tasks_sonar
#' @description
#' A classification task for the [mlbench::Sonar] data set.
#' Positive class is set to "M" (Mine).
#' @include mlr_tasks.R
mlr_tasks$add("sonar",  function(id = "sonar") {
  b = as_data_backend(load_dataset("Sonar", "mlbench"))
  b$hash = "_mlr_tasks_sonar_"
  TaskClassif$new(id, b, target = "Class", positive = "M")
})


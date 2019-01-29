#' @title Iris Classification Task
#' @name mlr_tasks_iris
#' @description
#' A classification task for the popular [datasets::iris] data set.
#' @include mlr_tasks.R
mlr_tasks$add("iris", function() {
  b = as_data_backend(load_dataset("iris", "datasets"))
  b$hash = "_mlr_tasks_iris_"
  TaskClassif$new("iris", b, target = "Species")
})


#' @title Iris Classification Task
#'
#' @name mlr_tasks_iris
#' @format [R6::R6Class] inheriting from [TaskClassif].
#' @include mlr_tasks.R
#'
#' @description
#' A classification task for the popular [datasets::iris] data set.
mlr_tasks$add("iris", function(id = "iris") {
  b = as_data_backend(load_dataset("iris", "datasets"))
  b$hash = "_mlr_tasks_iris_"
  TaskClassif$new(id, b, target = "Species")
})


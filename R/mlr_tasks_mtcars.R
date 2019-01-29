#' @title "Motor Trend" Car Road Tests Task
#' @name mlr_tasks_mtcars
#' @description
#' A regression task for the [datasets::mtcars] data set.
#' Target variable is `mpg` (Miles/(US) gallon).
#' @include mlr_tasks.R
mlr_tasks$add("mtcars",  function() {
  b = as_data_backend(load_dataset("mtcars", "datasets"))
  b$hash = "_mlr_tasks_mtcars_"
  TaskRegr$new("mtcars", b, target = "mpg")
})

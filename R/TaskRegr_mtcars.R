#' @title "Motor Trend" Car Road Tests Task
#'
#' @name mlr_tasks_mtcars
#' @format [R6::R6Class] inheriting from [TaskRegr].
#' @include mlr_tasks.R
#'
#' @section Construction:
#' ```
#' mlr_tasks$get("mtcars")
#' tsk("mtcars")
#' ```
#'
#' @description
#' A regression task for the [datasets::mtcars] data set.
#' Target variable is `mpg` (Miles/(US) gallon).
#'
#' @template seealso_task
NULL

load_task_mtcars = function(id = "mtcars") {
  b = as_data_backend(load_dataset("mtcars", "datasets", keep_rownames = TRUE))
  b$hash = "_mlr3_tasks_mtcars_"
  TaskRegr$new(id, b, target = "mpg")
}

#' @include mlr_tasks.R
mlr_tasks$add("mtcars", load_task_mtcars)

#' @title Motor Trend Regression Task
#'
#' @name mlr_tasks_mtcars
#' @format [R6::R6Class] inheriting from [TaskRegr].
#' @include mlr_tasks.R
#'
#' @description
#' A regression task for the [datasets::mtcars] data set.
#' Target variable is `mpg` (Miles/(US) gallon).
#' Rownames are stored as variable `"..rownames` with column role `"model"`.
#'
#' @section Construction:
#' ```
#' mlr_tasks$get("mtcars")
#' tsk("mtcars")
#' ```
#'
#' @section Meta Information:
#' `r rd_info(tsk("mtcars"))`
#'
#' @template seealso_task
NULL

load_task_mtcars = function(id = "mtcars") {
  b = as_data_backend(load_dataset("mtcars", "datasets", keep_rownames = TRUE), keep_rownames = "model")
  b$hash = "_mlr3_tasks_mtcars_"
  task = TaskRegr$new(id, b, target = "mpg")
  task$col_roles$name = "model"
  task$col_roles$feature = setdiff(task$col_roles$feature, "model")
  task
}

#' @include mlr_tasks.R
mlr_tasks$add("mtcars", load_task_mtcars)

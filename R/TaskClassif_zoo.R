#' @title Zoo Classification Task
#'
#' @name mlr_tasks_zoo
#' @format [R6::R6Class] inheriting from [TaskClassif].
#' @include mlr_tasks.R
#'
#' @description
#' A classification task for the [mlbench::Zoo] data set.
#' Rownames are stored as variable `"..rownames"` with column role `"name"`.
#'
#' @section Construction:
#' ```
#' mlr_tasks$get("zoo")
#' tsk("zoo")
#' ```
#'
#' @section Meta Information:
#' `r rd_info(tsk("zoo"))`
#'
#' @template seealso_task
NULL

load_task_zoo = function(id = "zoo") {
  b = as_data_backend(load_dataset("Zoo", "mlbench", keep_rownames = TRUE), keep_rownames = "animal")
  task = TaskClassif$new(id, b, target = "type")
  b$hash = task$man = "mlr3::mlr_tasks_zoo"
  task$col_roles$name = "animal"
  task$col_roles$feature = setdiff(task$col_roles$feature, "animal")
  task
}

#' @include mlr_tasks.R
mlr_tasks$add("zoo", load_task_zoo)

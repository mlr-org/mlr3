#' @title Zoo Classification Task
#' @name mlr_tasks_zoo
#' @description
#' A classification task for the [mlbench::Zoo] data set.
#' @include mlr_tasks.R
mlr_tasks$add("zoo", function() {
  b = as_data_backend(load_dataset("Zoo", "mlbench", keep_rownames = TRUE))
  b$hash = "_mlr_tasks_zoo_"
  TaskClassif$new("zoo", b, target = "type")
})

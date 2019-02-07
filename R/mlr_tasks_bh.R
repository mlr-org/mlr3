#' @title Boston Housing Regression Task
#' @name mlr_tasks_bh
#' @description
#' A regression task for the [mlbench::BostonHousing2] data set.
#' @include mlr_tasks.R
mlr_tasks$add("bh",  function(id = "bh") {
  b = as_data_backend(load_dataset("BostonHousing2", "mlbench"))
  b$hash = "_mlr_tasks_bh_"
  TaskRegr$new(id, b, target = "medv")
})

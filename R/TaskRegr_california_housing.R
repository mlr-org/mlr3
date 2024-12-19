#' @title Median House Value in California
#'
#' @name california_housing
#' @format [R6::R6Class] inheriting from [TaskRegr].
#' @aliases mlr_tasks_california_housing
#'
#' @description
#' A regression task to predict the median house value in California.
#'
#' Contains 9 features and 20640 observations.
#' Target column is `"median_house_value"`.
#'
#' @section Construction:
#' ```
#' mlr_tasks$get("california_housing")
#' tsk("california_housing")
#' ```
#'
#' @section Meta Information:
#' `r rd_info(tsk("california_housing"))`
#'
#' @source \url{https://www.kaggle.com/datasets/camnugent/california-housing-prices}
#'
#' @template seealso_task
NULL

load_task_california_housing = function(id = "california_housing") {
  b = as_data_backend(readRDS(system.file("extdata", "california_housing.rds", package = "mlr3")))
  task = mlr3::TaskRegr$new(id, b, target = "median_house_value", label = "California House Value")
  b$hash = task$man = "mlr3::mlr_tasks_california_housing"
  task
}

#' @include mlr_tasks.R
mlr_tasks$add("california_housing", load_task_california_housing)

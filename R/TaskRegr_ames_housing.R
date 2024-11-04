#' @title House Sales in Ames, Iowa
#'
#' @name ames_housing
#' @format [R6::R6Class] inheriting from [TaskRegr].
#' @aliases mlr_tasks_ames_housing
#'
#' @description
#' A regression task to predict house sale prices for Ames, Iowa.
#' This is the processed version from the [AmesHousing::make_ames()] package.
#'
#' Contains 80 features and 2930 observations.
#' Target column is `"Sale_Price"`.
#'
#' @section Construction:
#' ```
#' mlr_tasks$get("ames_housing")
#' tsk("ames_housing")
#' ```
#'
#' @section Meta Information:
#' `r rd_info(tsk("ames_housing"))`
#'
#' @template seealso_task
NULL

load_task_ames_housing = function(id = "ames_housing") {
  b = as_data_backend(readRDS(system.file("extdata", "ames_housing.rds", package = "mlr3")))
  task = mlr3::TaskRegr$new(id, b, target = "Sale_Price", label = "Ames House Sales")
  b$hash = task$man = "mlr3::mlr_tasks_ames_housing"
  task
}

#' @include mlr_tasks.R
mlr_tasks$add("ames_housing", load_task_ames_housing)


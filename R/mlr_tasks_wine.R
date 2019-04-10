#' @title Spam Classification Task
#'
#' @name mlr_tasks_wine
#' @format [R6::R6Class] inheriting from [TaskClassif].
#' @include mlr_tasks.R
#'
#' @section Usage:
#' ```
#' mlr_tasks$get("wine")
#' ```
#'
#' @description
#' A classification task for the [rattle.data::wine] data set.
mlr_tasks$add("wine", function(id = "wine") {
  data = load_dataset("wine", "rattle.data")
  names(data) = tolower(names(data))
  b = as_data_backend(data)
  b$hash = "_mlr3_tasks_wine_"
  TaskClassif$new(id, b, target = "type")
})

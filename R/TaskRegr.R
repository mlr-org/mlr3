#' @title Regression Tasks
#' @format [R6Class()] object
#'
#' @description
#' A [R6::R6Class()] to construct regression tasks.
#'
#' @template fields-task
#' @template fields-supervisedtask
#'
#' @return [TaskRegr()].
#' @include TaskSupervised.R
#' @family Tasks
#' @export
#' @examples
#' task = TaskRegr$new("iris", data = iris, target = "Sepal.Length")
#' task$formula
TaskRegr = R6Class("TaskRegr",
  inherit = TaskSupervised,
  public = list(
    initialize = function(id, backend, target) {
      super$initialize(id = id, backend = backend, target = target)
      assert_numeric(self$truth()[[1L]], finite = TRUE, any.missing = FALSE, .var.name = "target column")
      self$measures = mlr_measures$mget("mse")
    }
  )
)

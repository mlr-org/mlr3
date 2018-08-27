#' @title Regression Tasks
#' @format \code{\link{R6Class}} object
#'
#' @description
#' A \code{\link[R6]{R6Class}} to construct regression tasks.
#'
#' @template fields-task
#' @template fields-supervisedtask
#'
#' @return [\code{\link{TaskRegr}}].
#' @include TaskSupervised.R
#' @family Tasks
#' @export
#' @examples
#' task = TaskRegr$new(data = iris, target = "Sepal.Length")
#' task$formula
TaskRegr = R6Class("TaskRegr",
  inherit = TaskSupervised,
  public = list(
    task_type = "regr",
    default_measure = "mse",
    default_prediction = NA_real_,

    initialize = function(id, data, target) {
      super$initialize(id = id, data = data, target = target)
      assert_numeric(self$truth()[[1L]], finite = TRUE, any.missing = FALSE, .var.name = "target column")
    }
  ),

  active = list(
    summary = function() {
      summary(self$data(cols = self$target_names)[[1L]])
    }
  )
)

#' @title Abstract Prediction Object
#'
#' @description
#' This is the abstract base class for task objects like [PredictionClassif] and [PredictionRegr].
#'
#' @export
#' @name Prediction
#' @family Prediction
#' @keywords internal
NULL

Prediction = R6Class("Prediction",
  public = list(
    response = NULL,
    truth = NULL
  )
)

#' @export
as.data.table.Prediciton = function(x, ...) {
  data.table(response = x$response, truth = x$truth)
}

#' @export
as.data.frame.Prediciton = function(x, ...) {
  setDF(as.data.table(x))[]
}

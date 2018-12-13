#' @title Abstract Prediction Object
#'
#' @description
#' This is the abstract base class for task objects like [PredictionClassif] and [PredictionRegr].
#'
#' @export
#' @name Prediction
#' @family Prediction
NULL

Prediction = R6Class("Prediction",
  public = list(
    row_ids = NULL,
    predict_types = NULL,
    truth = NULL,
    response = NULL,

    print = function(...) {
      data = as.data.table(self)
      catf("<%s> for %i observations:", class(self)[1L], nrow(data))
      print(data, nrows = 10L, topn = 3L, print.class = TRUE, print.keys = FALSE)
    }
  )
)

#' @export
as.data.table.Prediction = function(x, ...) {
  data.table(row_id = x$row_ids, response = x$response, truth = x$truth)
}

#' @export
as.data.frame.Prediction = function(x, ...) {
  setDF(as.data.table(x))[]
}

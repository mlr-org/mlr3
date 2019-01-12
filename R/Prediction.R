#' @title Abstract Prediction Object
#'
#' @name Prediction
#' @format [R6Class] object.
#' @description
#' This is the abstract base class for task objects like [PredictionClassif] and [PredictionRegr].
#'
#' @family Prediction
NULL

#' @export
Prediction = R6Class("Prediction",
  public = list(
    row_ids = NULL,
    truth = NULL,
    response = NULL,
    predict_types = NULL,

    format = function() {
      sprintf("<%s>", class(self)[1L])
    },

    print = function(...) {
      if (is.null(self$row_ids)) {
        catf("%s for 0 observations", format(self))
      } else {
        data = as.data.table(self)
        catf("%s for %i observations:", format(self), nrow(data))
        print(data, nrows = 10L, topn = 3L, print.class = TRUE, print.keys = FALSE)
      }
    }
  )
)

#' @export
as.data.table.Prediction = function(x, ...) {
  if (is.null(x$row_ids))
    return(data.table())
  if (is.null(x$response) || is.null(x$truth))
    stop("Cannon convert Prediction to data.table: Prediction object incomplete")
  data.table(row_id = x$row_ids, response = x$response, truth = x$truth)
}

#' @export
as.data.frame.Prediction = function(x, ...) {
  setDF(as.data.table(x))[]
}

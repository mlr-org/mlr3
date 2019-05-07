#' @title Abstract Prediction Object
#'
#' @usage NULL
#' @format [R6::R6Class] object.
#'
#' @description
#' This is the abstract base class for task objects like [PredictionClassif] and [PredictionRegr].
#'
#' @section Construction:
#' ```
#' Prediction$new()
#' ```
#'
#' @section Fields:
#' * `row_ids` :: (`integer()` | `character()`)\cr
#'   Vector of row ids for which predictions are stored.
#'
#' * `truth` :: `any`\cr
#'   True (observed) outcome.
#'
#' * `response` :: `any`\cr
#'   Predicted outcome.
#'
#' * `task_type` :: `character(1)`\cr
#'    Stores the type of the [Task].
#'
#' * `predict_types` :: `character()`\cr
#'   Vector of predict types this object stores.
#'
#' @section S3 Methods:
#' * `as.data.table(rr)`\cr
#'   [BenchmarkResult] -> [data.table::data.table()]\cr
#'   Converts the data to a `data.table()`.
#'
#' @export
#' @family Prediction
Prediction = R6Class("Prediction",
  public = list(
    row_ids = NULL,
    truth = NULL,
    response = NULL,
    task_type = NULL,
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

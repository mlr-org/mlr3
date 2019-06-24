#' @title Abstract Prediction Object
#'
#' @usage NULL
#' @format [R6::R6Class] object.
#'
#' @description
#' This is the abstract base class for task objects like [PredictionClassif] or [PredictionRegr].
#'
#' Prediction objects store the following information:
#' 1. The row ids of the test set
#' 2. The corresponding true (observed) response.
#' 3. The corresponding predicted response.
#' 4. Additional predictions based on the class and `predict_type`.
#'    E.g., the class probabilities for classification or the estimated standard error for regression.
#'
#' @section Construction:
#' This object is constructed via a derived classes, e.g. [PredictionClassif] or [PredictionRegr].
#'
#' @section Fields:
#' * `row_ids` :: (`integer()` | `character()`)\cr
#'   Vector of row ids for which predictions are stored.
#'
#' * `truth` :: `any`\cr
#'   True (observed) outcome.
#'
#' * `task_type` :: `character(1)`\cr
#'    Stores the type of the [Task].
#'
#' * `predict_types` :: `character()`\cr
#'   Vector of predict types this object stores.
#'
#'
#' @section S3 Methods:
#' * `as.data.table(rr)`\cr
#'   [Prediction] -> [data.table::data.table()]\cr
#'   Converts the data to a `data.table()`.
#'
#' * `rbind(...)`\cr
#'   [([Prediction], [Prediction], ...)] -> [Prediction]\cr
#'   Combines multiple `Prediction`s to a single `Prediction`
#'
#' @export
#' @family Prediction
Prediction = R6Class("Prediction",
  public = list(
    data = list(),
    task_type = NULL,

    format = function() {
      sprintf("<%s>", class(self)[1L])
    },

    print = function(...) {
      if (is.null(self$data$row_ids)) {
        catf("%s for 0 observations", format(self))
      } else {
        data = as.data.table(self)
        catf("%s for %i observations:", format(self), nrow(data))
        print(data, nrows = 10L, topn = 3L, print.class = TRUE, print.keys = FALSE)
      }
    }
  ),

  active = list(
    predict_types = function() {
      setdiff(names(self$data), c("row_ids", "truth"))
    }
  )
)

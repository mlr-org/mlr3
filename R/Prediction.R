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
    row_ids = NULL,
    truth = NULL,
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

#' @title Prediction Object Helpers
#'
#' `convert_prediction()` is called on the slave to check and convert the return
#' value of the `predict()` function of a [Learner].
#'
#' `as_prediction()` is used to construct a [Prediction] object from the data
#' returned by `convert_prediction()`.
#'
#' @param task ([Task])\cr
#'  Task as passed to `$predict()` of a [Learner].
#' @param predicted (named `list()`)\cr
#'  Return value of the [Learner].
#' @param row_ids (`integer()` | `character()`)\cr
#'  Row ids of the [Task] for the observations in the test set.
#' @export
convert_prediction = function(task, predicted) {
  UseMethod("convert_prediction")
}

#' @rdname convert_prediction
#' @export
as_prediction = function(task, row_ids, predicted) {
  UseMethod("as_prediction")
}

#' @export
convert_prediction.default = function(task, predicted) {
  predicted
}

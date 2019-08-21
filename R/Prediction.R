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
#' * `missing` :: `logical()`\cr
#'   Returns `row_ids` for which the predictions are missing or incomplete.
#'
#' @section Methods:
#' * `score(measures = NULL, task = NULL, learner = NULL)`\cr
#'   (list of [Measure], [Task], [Learner]) -> [Prediction]\cr
#'   Calculates the performance for all provided measures
#'   [Task] and [Learner] may be `NULL` for most measures, but some measures need to extract information
#'   from these objects.
#'
#' @section S3 Methods:
#' * `as.data.table(rr)`\cr
#'   [Prediction] -> [data.table::data.table()]\cr
#'   Converts the data to a [data.table::data.table()].
#'
#' * `c(..., keep_duplicates = TRUE)`\cr
#'   ([Prediction], [Prediction], ...) -> [Prediction]\cr
#'   Combines multiple `Prediction`s to a single `Prediction`.
#'   If `keep_duplicates` is `FALSE` and there are duplicated row ids,
#'   the data of the former passed objects get overwritten by the data of the later passed objects.
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
        print(data, nrows = 10L, topn = 3L, class = FALSE, row.names = FALSE, print.keys = FALSE)
      }
    },

    score = function(measures = NULL, task = NULL, learner = NULL, train_set = NULL) {
      measures = assert_measures(as_measures(measures, task_type = self$task_type), task = task, learner = learner)
      scores = map_dbl(measures, function(m) m$score(prediction = self, task = task, learner = learner, train_set = train_set))
      set_names(scores, ids(measures))
    }
  ),

  active = list(
    row_ids = function() self$data$row_ids,
    truth = function() self$data$truth,
    predict_types = function() setdiff(names(self$data), c("row_ids", "truth")),
    missing = function() {
      self$data$row_ids[0L]
    } # empty vector
  )
)

#' @export
c.Prediction = function(..., keep_duplicates = TRUE) {
  cl = class(list(...)[[1L]])[1L]
  stopf("c.Prediction not implemented for '%s'", cl)
}

#' @title Abstract Prediction Object
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
#' Note that this object is usually constructed via a derived classes, e.g. [PredictionClassif] or [PredictionRegr].
#'
#' @template param_measures
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
    #' @field data (named `list()`)\cr
    #' Internal data structure.
    data = list(),

    #' @field task_type (`character(1)`)\cr
    #' Required type of the [Task].
    task_type = NULL,

    #' @field task_properties (`character()`)\cr
    #' Required properties of the [Task].
    task_properties = NULL,

    #' @field predict_types (`character()`)\cr
    #' Set of predict types this object stores.
    predict_types = character(),

    #' @template field_man
    man = "mlr3::Prediction",

    #' @description
    #' Helper for print outputs.
    format = function() {
      sprintf("<%s>", class(self)[1L])
    },

    #' @description
    #' Printer.
    #' @param ... (ignored).
    print = function(...) {
      n = length(self$data$row_ids)
      if (n == 0L) {
        catf("%s for 0 observations", format(self))
      } else {
        data = as.data.table(self)
        catf("%s for %i observations:", format(self), n)
        print(data, nrows = 10L, topn = 3L, class = FALSE, row.names = FALSE, print.keys = FALSE)
      }
    },

    #' @description
    #' Opens the corresponding help page referenced by field `$man`.
    help = function() {
      open_help(self$man)
    },

    #' @description
    #' Calculates the performance for all provided measures
    #' [Task] and [Learner] may be `NULL` for most measures, but some measures need to extract information
    #' from these objects.
    #'
    #' @param task ([Task]).
    #'
    #' @param learner ([Learner]).
    #'
    #' @param train_set (`integer()`).
    #'
    #' @return [Prediction].
    score = function(measures = NULL, task = NULL, learner = NULL, train_set = NULL) {
      measures = assert_measures(as_measures(measures, task_type = self$task_type), task = task, learner = learner)
      scores = map_dbl(measures, function(m) m$score(prediction = self, task = task, learner = learner, train_set = train_set))
      set_names(scores, ids(measures))
    }
  ),

  active = list(
    #' @field row_ids (`integer()`)\cr
    #'   Vector of row ids for which predictions are stored.
    row_ids = function(rhs) {
      assert_ro_binding(rhs)
      self$data$row_ids
    },

    #' @field truth (`any`)\cr
    #'   True (observed) outcome.
    truth = function(rhs) {
      assert_ro_binding(rhs)
      self$data$truth
    },

    #' @field missing (`integer()`)\cr
    #'   Returns `row_ids` for which the predictions are missing or incomplete.
    missing = function(rhs) {
      assert_ro_binding(rhs)
      self$data$row_ids[0L] # empty vector
    }
  )
)

#' @export
c.Prediction = function(..., keep_duplicates = TRUE) { # nolint
  dots = list(...)
  if (length(dots) == 1L) {
    return(dots[[1L]])
  }

  classes = unique(map_chr(dots, function(x) class(x)[1L]))
  if (length(classes) > 1L) {
    stopf("Cannot combine objects of different type: %s", str_collapse(classes))
  }
  assert_flag(keep_duplicates)

  pdata = invoke(c, .args = c(map(dots, "data"), list(keep_duplicates = keep_duplicates)))
  as_prediction(pdata)
}

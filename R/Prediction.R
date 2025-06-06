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
#' @template seealso_prediction
#' @export
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
    #' @param ... (ignored).
    format = function(...) {
      sprintf("<%s>", class(self)[1L])
    },

    #' @description
    #' Printer.
    #' @param ... (ignored).
    print = function(...) {
      n = length(self$data$row_ids)
      if (n == 0L) {
        cat_cli(cli_h1("{.cls {class(self)[1L]}} for {.val 0} observations"))
      } else {
        data = as.data.table(self)
        cat_cli(cli_h1("{.cls {class(self)[1L]}} for {.val {n}} observations:"))
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
    #' Note that the `predict_sets` of the `measures` are ignored by this method,
    #' instead all predictions are used.
    #'
    #' @param task ([Task]).
    #'
    #' @param learner ([Learner]).
    #'
    #' @param train_set (`integer()`).
    #'
    #' @return [Prediction].
    score = function(measures = NULL, task = NULL, learner = NULL, train_set = NULL) {
      measures = assert_measures(as_measures(measures, task_type = self$task_type))
      scores = map_dbl(measures, function(m) m$score(prediction = self, task = task, learner = learner, train_set = train_set))
      set_names(scores, ids(measures))
    },

    #' @description
    #' Calculates the observation-wise loss via the loss function set in the
    #' [Measure]'s field `obs_loss`.
    #' Returns a `data.table()` with the columns `row_ids`, `truth`, `response` and
    #' one additional numeric column for each measure, named with the respective measure id.
    #' If there is no observation-wise loss function for the measure, the column is filled with
    #' `NA` values.
    #' Note that some measures such as RMSE, do have an `$obs_loss`, but they require an
    #' additional transformation after aggregation, in this example taking the square-root.
    obs_loss = function(measures = NULL) {
      measures = assert_measures(as_measures(measures, task_type = self$task_type))
      get_obs_loss(as.data.table(self), measures)
    },


    #' @description
    #' Filters the [Prediction], keeping only predictions for the provided row_ids.
    #' This changes the object in-place, you need to create a clone to preserve
    #' the original [Prediction].
    #'
    #' @template param_row_ids
    #' @return `self`, modified.
    filter = function(row_ids) {
      row_ids = assert_row_ids(row_ids)
      self$data = filter_prediction_data(self$data, row_ids)
      invisible(self)
    }
  ),

  active = list(
    #' @field row_ids (`integer()`)\cr
    #'   Vector of row ids for which predictions are stored.
    row_ids = function(rhs) {
      assert_ro_binding(rhs)
      self$data$row_ids
    },

    #' @field truth (any)\cr
    #'   True (observed) outcome.
    truth = function(rhs) {
      assert_ro_binding(rhs)
      self$data$truth
    },

    #' @field missing (`integer()`)\cr
    #'   Returns `row_ids` for which the predictions are missing or incomplete.
    missing = function(rhs) {
      assert_ro_binding(rhs)
      is_missing_prediction_data(self$data)
    },

    #' @field weights (`numeric()`)\cr
    #'   Vector of measure weights, obtained from the `weights_measure` column of the [Task] if present.
    #'   This is `NULL` if no weights are present.
    weights = function(rhs) {
      assert_ro_binding(rhs)
      self$data$weights
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
  as_prediction(pdata, check = FALSE)
}

# #' @export
# format_list_item.Prediction = function(x, ...) { # nolint
#   sprintf("<prd[%i]>", length(x$row_ids))
# }

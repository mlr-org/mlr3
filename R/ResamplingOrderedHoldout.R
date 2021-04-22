#' @title Holdout Resampling for Ordered Data
#'
#' @name mlr_resamplings_ordered_holdout
#' @include Resampling.R
#'
#' @description
#' Splits data into a training set and a test set, according to columns with column role `order`.
#'
#' @templateVar id ordered_holdout
#' @template section_dictionary_resampling
#'
#' @section Parameters:
#' * `ratio` (`numeric(1)`)\cr
#'   Ratio of observations to put into the training set.
#'   Mutually exclusive with parameter `n`.
#' * `n` (`integer(1)`)\cr
#'   Number of observations to put into the training set.
#'   If negative, the absolute value determines the number of observations in the test set.
#'   Mutually exclusive with parameter `ratio`.
#' * `unit` (`character(1)`)\cr
#'   Either `"rows"` (default) to split based on the row number, or, if there is a single order column
#'   of type [POSIXct], one of `"secs"`, `"mins"`, `"hours"`, `"days"`, `"months"`, or `"years"`.
#'   For the latter, the ratio (`ratio`) or number of observations (`n`) correspond to the respective
#'   time unit.
#'
#' @template seealso_resampling
#' @export
#' @examples
#' # task airpassengers is ordered by monthly column 'date'
#' task = tsk("airpassengers")
#'
#' # predict on last 12 months
#' r = rsmp("ordered_holdout", n = -12)$instantiate(task)
#' r$train_set(1)
#' r$test_set(1)
ResamplingOrderedHoldout = R6Class("ResamplingOrderedHoldout", inherit = Resampling,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      ps = ps(
        ratio = p_dbl(0, 1),
        n = p_int(),
        unit = p_fct(levels = c("rows", "secs", "mins", "hours", "days", "months", "years"), default = "rows", tags = "required")
      )
      ps$values = list(unit = "rows")

      super$initialize(id = "ordered_holdout", param_set = ps, man = "mlr3::mlr_resamplings_ordered_holdout")
    },

    #' @template field_iters
    iters = 1L
  ),

  private = list(
    .sample = function(ids, task, ...) {
      if ("ordered" %nin% task$properties) {
        stopf("Resampling '%s' requires an ordered task, but Task '%s' has no order", self$id, task$id)
      }
      pv = self$param_set$values

      get_training_size = function(n_obs) {
        if (!xor(is.null(pv$ratio), is.null(pv$n))) {
          stopf("Either parameter `ratio` (x)or `n` must be provided")
        }

        if (!is.null(pv$ratio)) {
          round(n_obs * pv$ratio)
        } else {
          if (pv$n > 0L) {
            min(n_obs, pv$n)
          } else {
            max(n_obs + pv$n, 0L)
          }
        }
      }

      if (pv$unit == "rows") {
        ids = ids[task$order(ids)]
        n = get_training_size(length(ids))
        list(train = head(ids, n), test = tail(ids, -n))
      } else {
        order_cols = task$col_roles$order
        if (length(order_cols) != 1L || fget(task$col_info, order_cols, "type", "id") != "POSIXct") {
          stopf("For 'unit' != \"rows\", the task must be ordered by a single POSIXct column")
        }

        t = row_id = NULL
        tab = setnames(task$backend$data(rows = ids, cols = c(task$backend$primary_key, order_cols)), c("row_id", "t"))
        tab = tab[
          order(t),
          list(row_id = list(row_id)),
          by = as.POSIXct(trunc(t, units = pv$unit))]
        n = get_training_size(nrow(tab))
        list(train = unlist(head(tab$row_id, n), use.names = FALSE), test = unlist(tail(tab$row_id, -n), use.names = FALSE))
      }
    },

    .get_train = function(i) {
      self$instance$train
    },

    .get_test = function(i) {
      self$instance$test
    },

    .combine = function(instances) {
      list(train = do.call(c, map(instances, "train")), test = do.call(c, map(instances, "test")))
    })
)

#' @include mlr_resamplings.R
mlr_resamplings$add("ordered_holdout", ResamplingOrderedHoldout)

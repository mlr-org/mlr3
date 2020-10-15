#' @title Holdout Resampling for Ordered Data
#'
#' @name mlr_resamplings_ordered_holdout
#' @include Resampling.R
#'
#' @description
#' Splits data into a training set and a test set, according to columns with column role `order`.
#' Parameter `ratio` determines the ratio of observation going into the training set (default: 2/3).
#'
#' @templateVar id ordered_holdout
#' @template section_dictionary_resampling
#'
#' @section Parameters:
#' * `ratio` (`numeric(1)`)\cr
#'   Ratio of observations to put into the training set.
#'
#' @template seealso_resampling
#' @export
ResamplingOrderedHoldout = R6Class("ResamplingOrderedHoldout", inherit = Resampling,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      ps = ParamSet$new(list(
        ParamDbl$new("ratio", lower = 0, upper = 1, tags = "required")
      ))
      ps$values = list(ratio = 2 / 3)

      super$initialize(id = "ordered_holdout", param_set = ps, man = "mlr3::mlr_resamplings_ordered_holdout")
    },

    #' @template field_iters
    iters = 1L
  ),

  private = list(
    .sample = function(ids, task, ...) {
      if ("ordered" %nin% task$properties) {
        stopf("%s requires an ordered task, but Task '%s' has no order", self$id, task$id)
      }
      nr = round(length(ids) * self$param_set$values$ratio)
      ids = ids[task$order(ids)]
      list(train = head(ids, nr), test = tail(ids, -nr))
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

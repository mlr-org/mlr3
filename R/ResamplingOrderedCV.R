#' @title Cross Validation Resampling for Ordered Data
#'
#' @name mlr_resamplings_ordered_cv
#' @include Resampling.R
#'
#' @description
#' Successively splits data into training and test sets, according to columns with column role `order`.
#'
#'
#' @templateVar id ordered_holdout
#' @template section_dictionary_resampling
#'
#' @section Parameters:
#' * `train_window_width` (`integer(1)`)\cr
#'   Width of the window.
#' @template seealso_resampling
#' @export
ResamplingOrderedCV = R6Class("ResamplingOrderedCV", inherit = Resampling,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      ps = ParamSet$new(list(
        ParamInt$new("train_window_width", lower = 1L, tags = "required"),
        ParamInt$new("test_window_width", lower = 1L, default = 1L, tags = "required"),
      ))
      ps$values = list(window = 10, rolling = TRUE, horizon = 1L)

      super$initialize(id = "ordered_cv", param_set = ps, man = "mlr3::mlr_resamplings_ordered_cv")
    },

    #' @template field_iters
    iters = NA_integer_
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
mlr_resamplings$add("ordered_cv", ResamplingOrderedCV)

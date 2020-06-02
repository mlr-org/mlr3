#' @title Holdout Resampling
#'
#' @name mlr_resamplings_holdout
#' @include Resampling.R
#'
#' @description
#' Splits data into a training set and a test set.
#' Parameter `ratio` determines the ratio of observation going into the training set (default: 2/3).
#'
#' @templateVar id holdout
#' @template section_dictionary_resampling
#'
#' @section Parameters:
#' * `ratio` (`numeric(1)`)\cr
#'   Ratio of observations to put into the training set.
#'
#' @references
#' \cite{mlr3}{bischl_2012}
#'
#' @template seealso_resampling
#' @export
#' @examples
#' # Create a task with 10 observations
#' task = tsk("iris")
#' task$filter(1:10)
#'
#' # Instantiate Resampling
#' rho = rsmp("holdout", ratio = 0.5)
#' rho$instantiate(task)
#'
#' # Individual sets:
#' rho$train_set(1)
#' rho$test_set(1)
#' intersect(rho$train_set(1), rho$test_set(1))
#'
#' # Internal storage:
#' rho$instance # simple list
ResamplingHoldout = R6Class("ResamplingHoldout", inherit = Resampling,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      ps = ParamSet$new(list(
        ParamDbl$new("ratio", lower = 0, upper = 1, tags = "required")
      ))
      ps$values = list(ratio = 2 / 3)

      super$initialize(id = "holdout", param_set = ps, man = "mlr3::mlr_resamplings_holdout")
    },

    #' @template field_iters
    iters = 1L
  ),

  private = list(
    .sample = function(ids, ...) {
      nr = round(length(ids) * self$param_set$values$ratio)
      ii = shuffle(ids, nr)
      list(train = ii, test = setdiff(ids, ii))
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
mlr_resamplings$add("holdout", ResamplingHoldout)

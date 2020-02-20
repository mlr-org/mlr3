#' @title Bootstrap Resampling
#'
#' @name mlr_resamplings_bootstrap
#' @include Resampling.R
#'
#' @description
#' Splits data into bootstrap samples (sampling with replacement).
#' Hyperparameters are the number of bootstrap iterations (`repeats`, default: 30)
#' and the ratio of observations to draw per iteration (`ratio`, default: 1) for the training set.
#'
#' @templateVar id bootstrap
#' @template section_dictionary_resampling
#'
#' @section Parameters:
#' * `repeats` (`integer(1)`)\cr
#'   Number of repetitions.
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
#' rb = rsmp("bootstrap", repeats = 2, ratio = 1)
#' rb$instantiate(task)
#'
#' # Individual sets:
#' rb$train_set(1)
#' rb$test_set(1)
#' intersect(rb$train_set(1), rb$test_set(1))
#'
#' # Internal storage:
#' rb$instance$M # Matrix of counts
ResamplingBootstrap = R6Class("ResamplingBootstrap", inherit = Resampling,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      ps = ParamSet$new(list(
        ParamInt$new("repeats", lower = 1L, tags = "required"),
        ParamDbl$new("ratio", lower = 0, upper = 1, tags = "required"))
      )
      ps$values = list(ratio = 1, repeats = 30L)

      super$initialize(id = "bootstrap", param_set = ps, duplicated_ids = TRUE, man = "mlr3::mlr_resamplings_bootstrap")
    }
  ),

  active = list(
    #' @template field_iters
    iters = function(rhs) {
      assert_ro_binding(rhs)
      as.integer(self$param_set$values$repeats)
    }
  ),

  private = list(
    .sample = function(ids) {
      pv = self$param_set$values
      nr = round(length(ids) * pv$ratio)
      x = factor(seq_along(ids))
      M = replicate(pv$repeats, table(sample(x, nr, replace = TRUE)), simplify = "array")
      rownames(M) = NULL
      list(row_ids = ids, M = M)
    },

    .get_train = function(i) {
      rep(self$instance$row_ids, times = self$instance$M[, i])
    },

    .get_test = function(i) {
      self$instance$row_ids[self$instance$M[, i] == 0L]
    },

    .combine = function(instances) {
      list(row_ids = do.call(c, map(instances, "row_ids")), M = do.call(rbind, map(instances, "M")))
    }
  )
)

#' @include mlr_resamplings.R
mlr_resamplings$add("bootstrap", ResamplingBootstrap)

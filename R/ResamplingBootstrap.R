#' @title Bootstrap Resampling
#'
#' @aliases mlr_resamplings_bootstrap
#' @format [R6::R6Class] inheriting from [Resampling].
#' @include Resampling.R
#'
#' @description
#' Simple Bootstrap sampling.
#' Hyperparameters are the number of bootstrap iterations (`repeats`, default: 30)
#' and the ratio of observations to draw per iteration (`ratio`, default: 1).
#'
#' @section Fields:
#' See [Resampling].
#'
#' @section Methods:
#' See [Resampling].
#'
#' @export
#' @examples
#' # Create a task with 10 observations
#' task = mlr_tasks$get("iris")
#' task$filter(1:10)
#'
#' # Instantiate Resampling
#' rb = mlr_resamplings$get("bootstrap")
#' rb$param_set$values = list(repeats = 2, ratio = 1)
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
    initialize = function(id = "bootstrap", param_vals = list(ratio = 1, repeats = 30L)) {
      super$initialize(
        id = id,
        param_set = ParamSet$new(params = list(
          ParamUty$new("stratify", default = NULL),
          ParamInt$new("repeats", lower = 1L, tags = "required"),
          ParamDbl$new("ratio", lower = 0, upper = 1, tags = "required"))
        ),
        param_vals = param_vals,
        duplicated_ids = TRUE
      )
    }
  ),

  active = list(
    iters = function() {
      self$param_set$values$repeats
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

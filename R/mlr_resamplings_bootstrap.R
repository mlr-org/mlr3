#' @title Bootstrap Resampling
#'
#' @name mlr_resamplings_bootstrap
#' @format [R6::R6Class] inheriting from [Resampling].
#'
#' @description
#' Simple Bootstrap sampling.
#' You can control the number of bootstrap iterations (`repeats`)
#' and the number of observations to draw per iteration (`ratio`).
#'
#' @include Resampling.R
#' @export
#' @examples
#' # Create a task with 10 observations
#' task = mlr_tasks$get("iris")
#' task$filter(1:10)
#'
#' # Instantiate Resampling
#' rb = mlr_resamplings$get("bootstrap")
#' rb$param_vals = list(repeats = 2, ratio = 1)
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
    initialize = function(id = "bootstrap") {
      super$initialize(
        id = id,
        param_set = ParamSet$new(params = list(
            ParamInt$new("repeats", lower = 1L, tags = "required"),
            ParamDbl$new("ratio", lower = 0, upper = 1, tags = "required"))
        ),
        param_vals = list(ratio = 1, repeats = 30L),
        duplicated_ids = TRUE
      )
    }
  ),

  active = list(
    iters = function() {
      self$param_vals$repeats
    }
  ),

  private = list(
    .sample = function(ids) {
      nr = round(length(ids) * self$param_vals$ratio)
      x = factor(seq_along(ids))
      M = replicate(self$param_vals$repeats, table(sample(x, nr, replace = TRUE)), simplify = "array")
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

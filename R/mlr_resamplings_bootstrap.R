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
        param_vals = list(ratio = 1, repeats = 30L)
      )
      self$has_duplicates = TRUE
    },

    instantiate = function(task, ...) {
      assert_task(task)
      private$.instantiate(task, instantiate_bootstrap(task, self$param_vals$ratio, self$param_vals$repeats, self$stratify))
    },

    train_set = function(i) {
      i = assert_resampling_index(self, i)
      rep(self$instance$row_ids, times = self$instance$M[, i])
    },

    test_set = function(i) {
      i = assert_resampling_index(self, i)
      self$instance$row_ids[self$instance$M[, i] == 0L]
    }
  ),

  active = list(
    iters = function() {
      self$param_vals$repeats
    }
  )
)


#' @include mlr_resamplings.R
mlr_resamplings$add("bootstrap", ResamplingBootstrap)


resample_bootstrap = function(ids, ratio, repeats) {
  nr = round(length(ids) * ratio)
  x = factor(seq_along(ids))
  M = replicate(repeats, table(sample(x, nr, replace = TRUE)), simplify = "array")
  rownames(M) = NULL
  list(row_ids = ids, M = M)
}

instantiate_bootstrap = function(task, ratio, repeats, stratify = character(0L)) {
  if (length(stratify) == 0L) {
    res = resample_bootstrap(task$row_ids[[1L]], ratio, repeats)
  } else {
    grps = stratify_groups(task, stratify = stratify)
    res = lapply(grps$..row_id, resample_bootstrap, ratio = ratio, repeats = repeats)
    res = list(row_ids = do.call(c, map(res, "row_ids")), M = do.call(rbind, map(res, "M")))
  }

  res
}

#' @include Resampling.R
ResamplingBootstrap = R6Class("ResamplingBootstrap", inherit = Resampling,
  public = list(
    initialize = function(id = "bootstrap") {
      super$initialize(
        id = id,
        par_set = ParamSet$new(params = list(
            ParamInt$new("repeats", lower = 1L),
            ParamReal$new("ratio", lower = 0, upper = 1))
        ),
        par_vals = list(ratio = 1, repeats = 30L)
      )
      self$has_duplicates = TRUE
    },

    instantiate = function(task, ...) {
      assert_task(task)
      private$.hash = NA_character_
      self$instance = resampling_bootstrap(task$row_ids(), self$par_vals$ratio, self$par_vals$repeats)
      self
    },

    train_set = function(i) {
      i = assert_resampling_index(self, i)
      self$instance[[i]]$train
    },

    test_set = function(i) {
      i = assert_resampling_index(self, i)
      self$instance[[i]]$test
    }
  ),

  active = list(
    iters = function() {
      self$par_vals$repeats
    }
  )
)


#' @include mlr_resamplings.R
mlr_resamplings$add("bootstrap", ResamplingBootstrap)


resampling_bootstrap = function(ids, ratio, repeats) {
  n = length(ids)
  nr = as.integer(round(n * ratio))
  replicate(repeats, {
    ii = sort(sample.int(n, nr, replace = TRUE))
    list(train = ids[ii], test = ids[-unique(ii)])
  }, simplify = FALSE)
}

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
    },

    instantiate = function(task, ...) {
      # inner function so we can easily implement blocking here
      # -> replace ids with unique values of blocking variable
      # -> join ids using blocks
      bootstrap = function(ids, ratio, repeats) {
        n = length(ids)
        nr = as.integer(n * ratio)
        replicate(repeats, {
          ii = sort(sample.int(n, nr, replace = TRUE))
          list(train = ids[ii], test = ids[-unique(ii)])
        }, simplify = FALSE)
      }

      assert_task(task)
      row_ids = task$row_ids()
      private$.instance = bootstrap(task$row_ids(), self$par_vals$ratio, self$par_vals$repeats)
      self
    },

    train_set = function(i) {
      i = assert_resampling_index(self, i)
      private$.instance[[i]]$train
    },

    test_set = function(i) {
      i = assert_resampling_index(self, i)
      private$.instance[[i]]$test
    }
  ),

  active = list(
    iters = function() {
      self$par_vals$repeats
    }
  )
)

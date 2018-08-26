#' @include Resampling.R
ResamplingBootstrap = R6Class("ResamplingBootstrap", inherit = Resampling,
  public = list(
    id = "bootstrap",
    ratio = 1,
    repeats = 30L,
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

      assertTask(task)
      row.ids = task$row.ids()
      private$instance = bootstrap(task$row.ids(), assertNumber(self$ratio, lower = 0), asInt(self$repeats))
      self
    },

    train.set = function(i) {
      i = assertResamplingIndex(self, i)
      private$instance[[i]]$train
    },

    test.set = function(i) {
      i = assertResamplingIndex(self, i)
      private$instance[[i]]$test
    }
  ),

  active = list(
    iters = function() {
      self$repeats
    }
  )
)

mlr.resamplings$add(
  ResamplingBootstrap$new()
)

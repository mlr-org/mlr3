#' @include Resampling.R
ResamplingSubsampling = R6Class("ResamplingSubsampling", inherit = Resampling,
  public = list(
    initialize = function(id = "subsampling") {
      super$initialize(
        id = id,
        par_set = ParamSet$new(params = list(ParamInt$new("repeats", lower = 1), ParamReal$new("ratio", lower = 0, upper = 1))),
        par_vals = list(repeats = 30L, ratio = 2/3)
      )
      self$has_duplicates = FALSE
    },
    instantiate = function(task, ...) {
      # inner function so we can easily implement blocking here
      # -> replace ids with unique values of blocking variable
      # -> join ids using blocks
      ss = function(ids, ratio, repeats) {
        n = length(ids)
        nr = as.integer(round(n * ratio))

        train = replicate(repeats,
          bit::as.bit(replace(logical(n), sample.int(n, nr), TRUE)),
          simplify = FALSE)
        list(train = train, row_ids = ids)
      }

      assert_task(task)
      private$.instantiate(ss(task$row_ids(), self$par_vals$ratio, self$par_vals$repeats))
    },

    train_set = function(i) {
      i = assert_resampling_index(self, i)
      private$.instance$row_ids[bit::as.which(private$.instance$train[[i]])]
    },

    test_set = function(i) {
      i = assert_resampling_index(self, i)
      private$.instance$row_ids[bit::as.which(!private$.instance$train[[i]])]
    }
  ),
  active = list(
    iters = function() {
      self$par_vals$repeats
    }
  )
)

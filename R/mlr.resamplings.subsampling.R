#' @include Resampling.R
ResamplingSubsampling = R6Class("ResamplingSubsampling", inherit = Resampling,
  public = list(
    id = "subsampling",
    ratio = 0.67,
    repeats = 30L,
    instantiate = function(task, ...) {
      # inner function so we can easily implement blocking here
      # -> replace ids with unique values of blocking variable
      # -> join ids using blocks
      ss = function(ids, ratio, repeats) {
        n = length(ids)
        nr = as.integer(n * ratio)

        train = replicate(repeats,
          bit::as.bit(replace(logical(n), sample.int(n, nr), TRUE)),
          simplify = FALSE)
        list(train = train, row.ids = ids)
      }

      assertTask(task)
      private$instance = ss(task$row.ids(), assertNumber(self$ratio, lower = 0), asInt(self$repeats, lower = 1L))
      self
    },

    train.set = function(i) {
      i = assertResamplingIndex(self, i)
      private$instance$row.ids[bit::as.which(private$instance$train[[i]])]
    },

    test.set = function(i) {
      i = assertResamplingIndex(self, i)
      private$instance$row.ids[bit::as.which(!private$instance$train[[i]])]
    }
  ),
  active = list(
    iters = function() {
      self$repeats
    }
  )
)

mlr.resamplings$add(
  ResamplingSubsampling$new()
)

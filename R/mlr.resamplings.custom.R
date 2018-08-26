#' @include Resampling.R
ResamplingCustom = R6Class("ResamplingCustom", inherit = Resampling,
  public = list(
    id = "custom",

    instantiate = function(task, train.sets = NULL, test.sets = NULL) {

      if (!is.null(train.sets)) {
        # TODO: more assertions
        assertList(train.sets, types = "atomicvector", any.missing = FALSE)
        private$instance = list(train = train.sets, test = NULL)
        private$hash = NA_character_
      }

      if (!is.null(test.sets)) {
        # TODO: more assertions
        train = private$instance$train
        if (is.null(train))
          stop("Cannot set test set without training set")
        assertList(test.sets, types = "atomicvector", len = length(train), any.missing = FALSE)
        private$instance$test = test.sets
        private$hash = NA_character_
      }
      self
    },

    train.set = function(i) {
      i = assertResamplingIndex(self, i)
      private$instance$train[[i]]
    },

    test.set = function(i) {
      i = assertResamplingIndex(self, i)
      private$instance$test[[i]]
    }
  ),

  active = list(
    iters = function() {
      length(private$instance$train)
    }
  )
)

mlr.resamplings$add(
  ResamplingCustom$new()
)

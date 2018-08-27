#' @include Resampling.R
ResamplingCustom = R6Class("ResamplingCustom", inherit = Resampling,
  public = list(
    id = "custom",

    instantiate = function(task, train_sets = NULL, test_sets = NULL) {

      if (!is.null(train_sets)) {
        # TODO: more assertions
        assert_list(train_sets, types = "atomicvector", any.missing = FALSE)
        private$instance = list(train = train_sets, test = NULL)
        private$hash = NA_character_
      }

      if (!is.null(test_sets)) {
        # TODO: more assertions
        train = private$instance$train
        if (is.null(train))
          stop("Cannot set test_set without training set")
        assert_list(test_sets, types = "atomicvector", len = length(train), any.missing = FALSE)
        private$instance$test = test_sets
        private$hash = NA_character_
      }
      self
    },

    train_set = function(i) {
      i = assert_resampling_index(self, i)
      private$instance$train[[i]]
    },

    test_set = function(i) {
      i = assert_resampling_index(self, i)
      private$instance$test[[i]]
    }
  ),

  active = list(
    iters = function() {
      length(private$instance$train)
    }
  )
)

mlr_resamplings$add(
  ResamplingCustom$new()
)

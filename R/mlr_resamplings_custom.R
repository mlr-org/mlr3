#' @include Resampling.R
ResamplingCustom = R6Class("ResamplingCustom", inherit = Resampling,
  public = list(
    initialize = function(id = "custom") {
      super$initialize(id = id)
    },

    instantiate = function(task, train_sets = NULL, test_sets = NULL) {
      assert_task(task)
      if (is.null(train_sets) && is.null(test_sets))
        stopf("At least one of 'train_sets' or 'test_sets' must be provided")
      instance = private$.instance %??% list(train = NULL, test = NULL)

      if (!is.null(train_sets)) {
        # TODO: more assertions?
        assert_list(train_sets, types = "atomicvector", any.missing = FALSE)
        instance$train = train_sets
        self$has_duplicates = any(viapply(train_sets, anyDuplicated) > 0L)
      }

      if (!is.null(test_sets)) {
        # TODO: more assertions?
        assert_list(test_sets, types = "atomicvector", len = length(instance$train), any.missing = FALSE, unique = TRUE)
        if (is.null(instance$train))
          stopf("Cannot set test_set without train_set")
        instance$test = test_sets
      }

      private$.instantiate(instance)
    },

    train_set = function(i) {
      i = assert_resampling_index(self, i)
      private$.instance$train[[i]]
    },

    test_set = function(i) {
      i = assert_resampling_index(self, i)
      private$.instance$test[[i]]
    }
  ),

  active = list(
    iters = function() {
      length(private$.instance$train)
    }
  )
)

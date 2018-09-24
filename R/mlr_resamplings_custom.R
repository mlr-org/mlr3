#' @include Resampling.R
ResamplingCustom = R6Class("ResamplingCustom", inherit = Resampling,
  public = list(
    initialize = function(id = "custom") {
      super$initialize(id = id)
      self$has_duplicates = NA
    },

    instantiate = function(task, train_sets = NULL, test_sets = NULL) {
      assert_task(task)
      instance = resampling_custom(private$.instance, train_sets, test_sets)
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
    },

    hash = function() {
      if (is.null(private$.instance$train) || is.null(private$.instance$test))
        return(NA_character_)
      if (is.na(private$.hash))
        private$.hash = digest::digest(list(self$id, private$.par_vals, private$.instance), algo = "xxhash64")
      private$.hash
    }
  )
)

resampling_custom = function(instance, train_sets = NULL, test_sets = NULL) {
  if (is.null(train_sets) && is.null(test_sets))
    stopf("At least one of 'train_sets' or 'test_sets' must be provided")
  instance = instance %??% list(train = NULL, test = NULL)

  if (!is.null(train_sets)) {
    # TODO: more assertions?
    assert_list(train_sets, types = "atomicvector", any.missing = FALSE)
    instance$train = train_sets
  }

  if (!is.null(test_sets)) {
    if (is.null(instance$train))
      stopf("Cannot set test_set without train_set")
    assert_list(test_sets, types = "atomicvector", len = length(instance$train), any.missing = FALSE, unique = TRUE)
    instance$test = test_sets
  }

  return(instance)
}

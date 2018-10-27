#' @include Resampling.R
ResamplingCustom = R6Class("ResamplingCustom", inherit = Resampling,
  public = list(
    initialize = function(id = "custom") {
      super$initialize(id = id)
      self$has_duplicates = NA
    },

    instantiate = function(task, train_sets = NULL, test_sets = NULL) {
      assert_task(task)
      private$.hash = NA_character_
      self$instance = resampling_custom(self$instance, train_sets, test_sets)
      self
    },

    train_set = function(i) {
      i = assert_resampling_index(self, i)
      self$instance$train[[i]]
    },

    test_set = function(i) {
      i = assert_resampling_index(self, i)
      self$instance$test[[i]]
    }
  ),

  active = list(
    iters = function() {
      length(self$instance$train)
    },

    hash = function() {
      if (is.null(self$instance$test))
        return(NA_character_)
      super$hash
    }
  )
)


#' @include mlr_resamplings.R
mlr_resamplings$add("custom", ResamplingCustom)


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

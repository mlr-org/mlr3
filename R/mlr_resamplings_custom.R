#' @title Custom Resampling
#' @name mlr_resamplings_custom
#' @format [R6::R6Class()] inheriting from [Resampling].
#'
#' @description
#' A custom resampling class where the training and test indices
#' can be set manually.
#'
#' @export
#' @include Resampling.R
#' @examples
#' # Create a task with 10 observations
#' task = mlr_tasks$get("iris")
#' task$filter(1:10)
#'
#' # Instantiate Resampling
#' rc = mlr_resamplings$get("custom")
#' train_sets = list(1:5, 5:10)
#' test_sets = list(5:10, 1:5)
#' rc$instantiate(task, train_sets, test_sets)
#'
#' rc$train_set(1)
#' rc$test_set(1)
ResamplingCustom = R6Class("ResamplingCustom", inherit = Resampling,
  public = list(
    initialize = function(id = "custom") {
      super$initialize(id = id)
      self$has_duplicates = NA
    },

    instantiate = function(task, train_sets = NULL, test_sets = NULL) {
      assert_task(task)
      if (length(self$stratify))
        stopf("Cannot stratify custom resampling")
      private$.instantiate(instantiate_custom(self$instance, train_sets, test_sets))
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


instantiate_custom = function(instance, train_sets = NULL, test_sets = NULL) {
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

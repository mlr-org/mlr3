#' @title Custom Resampling
#'
#' @name mlr_resamplings_custom
#' @format [R6::R6Class] inheriting from [Resampling].
#' @include Resampling.R
#'
#' @description
#' A custom resampling class where the training and test indices
#' can be set manually.
#'
#' @section Fields:
#' @inheritSection Learner Fields
#'
#' @section Methods:
#' @inheritSection Learner Methods
#'
#' @export
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
    initialize = function() {
      super$initialize(id = "custom", duplicated_ids = TRUE)
    },

    instantiate = function(task, train_sets = NULL, test_sets = NULL) {
      assert_task(task)
      self$instance = instantiate_custom(self$instance, train_sets, test_sets)
      self$task_hash = task$hash
      invisible(self)
    }
  ),

  active = list(
    iters = function() {
      length(self$instance$train)
    },

    hash = function() {
      if (is.null(self$instance$test))
        return(NA_character_)
      hash(list(class(self), self$id, self$param_set$values, self$instance))
    }
  ),

  private = list(
    .get_train = function(i) {
      self$instance$train[[i]]
    },

    .get_test = function(i) {
      self$instance$test[[i]]
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

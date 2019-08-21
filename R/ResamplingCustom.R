#' @title Custom Resampling
#'
#' @aliases mlr_resamplings_custom
#' @format [R6::R6Class] inheriting from [Resampling].
#' @include Resampling.R
#'
#' @description
#' A custom resampling class where the training and test indices can be set manually.
#'
#' @section Fields:
#' See [Resampling].
#'
#' @section Methods:
#' See [Resampling].
#'
#' @export
#' @examples
#' # Create a task with 10 observations
#' task = tsk("iris")
#' task$filter(1:10)
#'
#' # Instantiate Resampling
#' rc = rsmp("custom")
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
      assert_task(as_task(task))
      assert_list(train_sets, types = "atomicvector", any.missing = FALSE)
      assert_list(test_sets, types = "atomicvector", len = length(train_sets), any.missing = FALSE, null.ok = TRUE)
      self$instance = list(train = train_sets, test = test_sets)
      self$task_hash = task$hash
      invisible(self)
    }
  ),

  active = list(
    iters = function() {
      length(self$instance$train)
    },

    hash = function() {
      if (is.null(self$instance$test)) {
        return(NA_character_)
      }
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

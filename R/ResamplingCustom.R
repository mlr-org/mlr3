#' @title Custom Resampling
#'
#' @name mlr_resamplings_custom
#' @include Resampling.R
#'
#' @description
#' Splits data into training and test sets using manually provided indices.
#'
#' @templateVar id custom
#' @template section_dictionary_resampling
#'
#' @template seealso_resampling
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
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      super$initialize(id = "custom", duplicated_ids = TRUE, man = "mlr3::mlr_resamplings_custom")
    },

    #' @description
    #' Instantiate this [Resampling] with custom splits into training and test set.
    #'
    #' @param task [Task]\cr
    #'   Mainly used to check if `train_sets` and `test_sets` are feasible.
    #'
    #' @param train_sets (list of `integer()`)\cr
    #'   List with row ids for training, one list element per iteration.
    #'   Must have the same length as `test_sets`.
    #'
    #' @param test_sets (list of `integer()`)\cr
    #'   List with row ids for testing, one list element per iteration.
    #'   Must have the same length as `train_sets`.
    instantiate = function(task, train_sets, test_sets) {
      task = assert_task(as_task(task))
      assert_list(train_sets, types = "atomicvector", any.missing = FALSE)
      assert_list(test_sets, types = "atomicvector", len = length(train_sets), any.missing = FALSE, null.ok = TRUE)
      self$instance = list(train = train_sets, test = test_sets)
      self$task_hash = task$hash
      self$task_nrow = task$nrow
      invisible(self)
    }
  ),

  active = list(
    #' @template field_iters
    iters = function(rhs) {
      assert_ro_binding(rhs)
      length(self$instance$train)
    },

    #' @template field_hash
    hash = function(rhs) {
      assert_ro_binding(rhs)
      if (is.null(self$instance$test)) {
        return(NA_character_)
      }
      hash(class(self), self$id, self$param_set$values, self$instance)
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

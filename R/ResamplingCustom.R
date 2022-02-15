#' @title Custom Resampling
#'
#' @name mlr_resamplings_custom
#' @include Resampling.R
#'
#' @description
#' Splits data into training and validation sets using manually provided indices.
#'
#' @templateVar id custom
#' @template resampling
#'
#' @template seealso_resampling
#' @export
#' @examples
#' # Create a task with 10 observations
#' task = tsk("penguins")
#' task$filter(1:10)
#'
#' # Instantiate Resampling
#' custom = rsmp("custom")
#' train_sets = list(1:5, 5:10)
#' validation_sets = list(5:10, 1:5)
#' custom$instantiate(task, train_sets, validation_sets)
#'
#' custom$train_set(1)
#' custom$validation_set(1)
ResamplingCustom = R6Class("ResamplingCustom", inherit = Resampling,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      super$initialize(id = "custom", duplicated_ids = TRUE, man = "mlr3::mlr_resamplings_custom")
    },

    #' @description
    #' Instantiate this [Resampling] with custom splits into training and validation set.
    #'
    #' @param task [Task]\cr
    #'   Mainly used to check if `train_sets` and `validation_sets` are feasible.
    #'
    #' @param train_sets (list of `integer()`)\cr
    #'   List with row ids for training, one list element per iteration.
    #'   Must have the same length as `validation_sets`.
    #'
    #' @param validation_sets (list of `integer()`)\cr
    #'   List with row ids for validation, one list element per iteration.
    #'   Must have the same length as `train_sets`.
    #'
    #' @param test_sets (list of `integer()`)\cr
    #'   Deprecated, use `validation_sets` instead.
    instantiate = function(task, train_sets, validation_sets = NULL, test_sets = NULL) {
      switch(to_decimal(c(is.null(validation_sets), is.null(test_sets))) + 1L,
        stop("Provide only 'validation_sets', not 'test_sets' and 'validation_sets'"),
        TRUE,
        {
          .Deprecated("test_sets", "mlr3", "The set 'test' has been renamed to 'validation'")
          validation_sets = test_sets
        },
        stop("'validation_sets' is missing")
      )

      task = assert_task(as_task(task))
      assert_list(train_sets, types = "atomicvector", any.missing = FALSE)
      assert_list(validation_sets, types = "atomicvector", len = length(train_sets), any.missing = FALSE, null.ok = TRUE)
      assert_subset(unlist(train_sets), task$row_ids)
      assert_subset(unlist(validation_sets), task$row_ids)
      self$instance = list(train = train_sets, validation = validation_sets)
      self$task_hash = task$hash
      self$task_nrow = task$nrow
      invisible(self)
    }
  ),

  active = list(
    #' @template field_iters
    iters = function(rhs) {
      assert_ro_binding(rhs)
      if (self$is_instantiated) length(self$instance$train) else NA_integer_
    }
  ),

  private = list(
    .get_train = function(i) {
      self$instance$train[[i]]
    },

    .get_validation = function(i) {
      self$instance$validation[[i]]
    }
  )
)

#' @include mlr_resamplings.R
mlr_resamplings$add("custom", ResamplingCustom)

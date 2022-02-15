#' @title Custom Cross-Validation
#'
#' @name mlr_resamplings_custom_cv
#' @include Resampling.R
#'
#' @description
#' Splits data into training and validation sets in a cross-validation fashion based
#' on a user-provided categorical vector.
#' This vector can be passed during instantiation either via an arbitrary factor `f`
#' with the same length as `task$nrow`, or via a single string `col` referring to a
#' column in the task.
#'
#' An alternative but equivalent approach using leave-one-out resampling is
#' showcased in the examples of [mlr_resamplings_loo].
#'
#' @templateVar id custom_cv
#' @template resampling
#'
#' @template seealso_resampling
#' @export
#' @examples
#' # Create a task with 10 observations
#' task = tsk("penguins")
#' task$filter(1:10)
#'
#' # Instantiate Resampling:
#' custom_cv = rsmp("custom_cv")
#' f = factor(c(rep(letters[1:3], each = 3), NA))
#' custom_cv$instantiate(task, f = f)
#' custom_cv$iters # 3 folds
#'
#' # Individual sets:
#' custom_cv$train_set(1)
#' custom_cv$validation_set(1)
#'
#' # Disjunct sets:
#' intersect(custom_cv$train_set(1), custom_cv$validation_set(1))
ResamplingCustomCV = R6Class("ResamplingCustomCV", inherit = Resampling,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      super$initialize(id = "custom_cv", duplicated_ids = FALSE, man = "mlr3::mlr_resamplings_custom_cv")
    },

    #' @description
    #' Instantiate this [Resampling] as cross-validation with custom splits.
    #'
    #' @param task [Task]\cr
    #'   Used to extract row ids.
    #'
    #' @param f (`factor()` | `character()`)\cr
    #'   Vector of type factor or character with the same length as `task$nrow`.
    #'   Row ids are split on this vector, each distinct value results in a fold.
    #'   Empty factor levels are dropped and row ids corresponding to missing values are removed,
    #'   c.f. [split()].
    #' @param col (`character(1)`)\cr
    #'   Name of the task column to use for splitting.
    #'   Alternative and mutually exclusive to providing the factor levels as a vector via
    #'   parameter `f`.
    instantiate = function(task, f = NULL, col = NULL) {
      task = assert_task(as_task(task))
      if (!xor(is.null(f), is.null(col))) {
        stopf("Either `f` or `col` must be provided")
      }

      if (!is.null(col)) {
        assert_choice(col, task$col_info$id)
        f = task$data(cols = col)[[1L]]
        assert_atomic_vector(f, all.missing = FALSE)
      } else {
        assert_factor(f, len = task$nrow, all.missing = FALSE)
      }

      self$instance = split(task$row_ids, f, drop = TRUE)
      self$task_hash = task$hash
      self$task_nrow = task$nrow
      invisible(self)
    }
  ),

  active = list(
    #' @template field_iters
    iters = function(rhs) {
      assert_ro_binding(rhs)
      if (self$is_instantiated) length(self$instance) else NA_integer_
    }
  ),

  private = list(
    .get_train = function(i) {
      unlist(self$instance[-i], use.names = FALSE) %??% integer()
    },

    .get_validation = function(i) {
      self$instance[[i]]
    }
  )
)

#' @include mlr_resamplings.R
mlr_resamplings$add("custom_cv", ResamplingCustomCV)

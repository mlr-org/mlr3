#' @title Custom Cross Validation
#'
#' @name mlr_resamplings_custom_cv
#' @include Resampling.R
#'
#' @description
#' Splits data into training and test sets in a cross-validation fashion.
#' Splits are defined by argument `split` provided during instantiation.
#'
#' `split` can either be an external factor vector with the same length as
#' `task$nrow` or a character vector specifying a feature within the task which will be
#' used for splitting.
#'
#' An alternative approach using leave-one-out is showcased in the examples of
#' [mlr_resamplings_loo].
#'
#' @templateVar id custom_cv
#' @template section_dictionary_resampling
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
#' split_f = factor(c(rep(letters[1:3], each = 3), NA))
#' custom_cv$instantiate(task, split = split_f)
#' custom_cv$iters # 3 folds
#'
#' # Individual sets:
#' custom_cv$train_set(1)
#' custom_cv$test_set(1)
#'
#' # Disjunct sets:
#' intersect(custom_cv$train_set(1), custom_cv$test_set(1))
ResamplingCustomCV = R6Class("ResamplingCustomCV", inherit = Resampling,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      super$initialize(id = "custom_cv", duplicated_ids = FALSE, man = "mlr3::mlr_resamplings_custom_cv")
    },

    #' @description
    #' Instantiate this [Resampling] as cross validation with custom splits.
    #'
    #' @param task [Task]\cr
    #'   Used to extract row ids.
    #'
    #' @param split (`factor()` | `character(1)`)\cr
    #'   Either an external factor vector with the same length as
    #'   `task$nrow`, or a single column of the task which will be
    #'   used for splitting.
    #'   Row ids are split on this factor, each factor level results in a fold.
    #'   Empty factor levels are dropped and row ids corresponding to missing values are removed,
    #'   c.f. [split()].
    instantiate = function(task, split) {
      task = assert_task(as_task(task))

      if (test_string(split)) {
        cols = fget(task$col_info, c("character", "factor", "ordered"), "id", "type")
        assert_choice(split, cols)
        split = task$data(cols = split)[[1L]]
      }
      assert_factor(split, empty.levels.ok = FALSE, len = task$nrow, all.missing = FALSE)

      self$instance = split(task$row_ids, split, drop = TRUE)
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

    .get_test = function(i) {
      self$instance[[i]]
    }
  )
)

#' @include mlr_resamplings.R
mlr_resamplings$add("custom_cv", ResamplingCustomCV)

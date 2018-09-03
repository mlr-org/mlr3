#' Abstract learner class
#'
#' Abstraction for learners.
#'
#' @section Usage:
#' ```
#' l = Learner$new()
#' l$id
#' l$
#' ```
#'
#' @section Arguments:
#' * `data` (`data.frame` or `data.table`).
#' * `primary_key` (`character(1)`): Name of the column in `data` which represents a unique
#'     row identifier (as integer or character). If `NULL`, a new column with integer indices is
#'     automatically created.
#'
#' @section Details:
#' `$new()` creates a new object of class [Backend].
#'
#' @name BackendDataTable
#' @family Backend
#' @examples
#' b = BackendDataTable$new(data = iris)
#' b$head()
#' b$data(rows = 100:101, cols = "Species")
#'
#' b$nrow
#' head(b$rownames)
#'
#' b$ncol
#' b$colnames
NULL

#' @export
Learner = R6Class("Learner",
  public = list(
    id = NULL,
    packages = NULL,
    par_set = NULL,
    properties = NULL,

    initialize = function(id, packages = character(0L), par_set = ParamSet$new(), properties = character(0L)) {
      self$id = assert_string(id, min.chars = 1L)
      self$packages = assert_character(packages, any.missing = FALSE, min.chars = 1L)
      self$par_set = assert_r6(par_set, "ParamSet")
      self$properties = assert_character(properties, any.missing = FALSE, min.chars = 1L)
    },

    print = function(...) {
     catf("Learner '%s' for %s", self$id, self$task_type)
     catf(stri_list("Properties: ", self$properties))
    }
  ),

  active = list(
    par_vals = function(rhs) {
      if (missing(rhs))
        return(private$.par_vals)
      assert_list(rhs, names = "unique")
      assert_subset(names(rhs), self$par_set$ids)
      private$.par_vals[names(rhs)] = rhs
    },

    predict_type = function(rhs) {
      if (missing(rhs))
        return(private$.predict_type)
      assert_choice(rhs, capabilities$predict_types[[self$task_type]], fmatch = TRUE)
      private$.predict_type = rhs
    }
  ),
  private = list(
    .par_vals = NULL,
    .predict_type = NULL
  )
)

assert_learner = function(learner, task = NULL) {
  assert_r6(learner, "Learner")
  if (!is.null(task)) {
    if (!identical(class(task)[1L], learner$task_type)) {
      stopf("Learner '%s' (type: %s) is not compatible with task '%s' (type: %s)",
        learner$id, learner$task_type, task$id, class(task)[1L])
    }
  }
  invisible(learner)
}

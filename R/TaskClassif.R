#' @title Classification task
#'
#' @description
#' This task specializes [Task] and [TaskSupervised] for classification problems.
#' The target column is assumed to be a factor.
#'
#' @section Usage:
#' ```
#' t = TaskClassif$new(id, backend, target, positive = NULL)
#'
#' t$class_names
#' t$class_n
#' ```
#'
#' @section Arguments:
#' * `positive` (`character(1)`):
#'   Name of the "positive" class for binary classification problems.
#'
#' @section Details:
#' `class_names` returns all class labels of the rows which `role == "use"`.
#' `class_n` returns the number of class labels of the rows which `role == "use"`.
#'
#' @name TaskClassif
#' @family Tasks
#' @examples
#' b = BackendDataTable$new(iris)
#' task = TaskClassif$new("iris", backend = b, target = "Species")
#' task$formula
#' task$class_names
NULL


#' @include TaskSupervised.R
#' @export
TaskClassif = R6Class("TaskClassif",
  inherit = TaskSupervised,
  public = list(
    positive = NA_character_,

    initialize = function(id, backend, target, positive = NULL) {
      super$initialize(id = id, backend = backend, target = target)
      assert_string(target) # check for length 1

      # FIXME: pick a type here
      assert_vector(self$truth()[[1L]], any.missing = FALSE, .var.name = "target column")

      if (!is.null(positive))
        self$positive = assert_choice(positive, self$class_names)
      self$measures = list(mlr_measures$get("mmce"))
    }
  ),

  active = list(
    class_names = function() as.character(unique(self$truth()[[1L]])),
    class_n = function() uniqueN(self$truth()[[1L]])
  )
)

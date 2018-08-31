#' @title Classification Tasks
#' @format [R6Class()] object
#'
#' @description
#' A [R6::R6Class()] to construct classification tasks.
#'
#' @template fields-task
#' @template fields-supervisedtask
#' @field positive (`character(1)`)\cr
#'  Only for binary classification: Level of the positive class (`NA` otherwise).
#' @field classes [`character()`]\cr
#'  Levels of class labels.
#' @field nclasses (`integer(1)`)\cr
#'  Number of levels of class labels.
#'
#' @return [TaskClassif()].
#' @include TaskSupervised.R
#' @export
#' @family Tasks
#' @examples
#' b = BackendDataTable$new(iris)
#' task = TaskClassif$new("iris", backend = b, target = "Species")
#' task$class_names
TaskClassif = R6Class("TaskClassif",
  inherit = TaskSupervised,
  public = list(
    positive = NA_character_,

    initialize = function(id, backend, target, positive = NULL) {
      super$initialize(id = id, backend = backend, target = target)
      if (!is.null(positive))
        self$positive = assert_choice(positive, self$class_names)
      self$measures = mlr_measures$mget("mmce")
    }
  ),

  active = list(
    class_names = function() as.character(unique(self$truth()[[1L]])),
    class_n = function() uniqueN(self$truth()[[1L]])
  )
)

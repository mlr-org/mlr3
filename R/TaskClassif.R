#' @title Classification Tasks
#' @format \code{\link{R6Class}} object
#'
#' @description
#' A \code{\link[R6]{R6Class}} to construct classification tasks.
#'
#' @template fields-task
#' @template fields-supervisedtask
#' @field positive [\code{character(1)}]\cr
#'  Only for binary classification: Level of the positive class (\code{NA} otherwise).
#' @field classes [\code{character()}]\cr
#'  Levels of class labels.
#' @field nclasses [\code{integer(1)}]\cr
#'  Number of levels of class labels.
#'
#' @return [\code{\link{TaskClassif}}].
#' @include TaskSupervised.R
#' @export
#' @family Tasks
#' @examples
#' task = TaskClassif$new("iris", data = iris, target = "Species")
#' task$formula
TaskClassif = R6Class("TaskClassif",
  inherit = TaskSupervised,
  public = list(
    task.type = "classif",
    default.measure = "mmce",
    default.prediction = NA_character_,
    positive = NA_character_,

    initialize = function(id, data, target, positive = NULL) {
      super$initialize(id = id, data = data, target = target)
      if (!is.null(positive)) {
        self$positive = assertChoice(positive, self$classes)
      }
    }
  ),

  active = list(
    classes = function() as.character(unique(self$data(cols = self$target)[[1L]])),
    nclasses = function() uniqueN(self$data(cols = self$target)[[1L]])
  )
)

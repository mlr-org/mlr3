#' @title Abstract measure class
#'
#' @description
#' Abstraction for performance measures.
#'
#' @section Usage:
#' ```
#' m = Measure$new()
#' m$id
#' m$packages
#' m$task_types
#' ```
#'
#' @section Details:
#' `$new()` creates a new object of class [Measure].
#' Predefined learners are stored in [mlr_measures].
#'
#' `$id` (`character(1)`) stores the identifier of the object.
#'
#' `$packages` (`character(1)`) stores the names of required packages.
#'
#' `$task_types` (`character`) stores the class names of tasks this measure can operate on.
#'
#' @name Measure
#' @keywords internal
#' @family Measure
NULL

#' @export
Measure = R6Class("Measure",
  public = list(
    id = NA_character_,
    task_types = NA_character_,
    packages = character(0L)
  )
)

assert_measure = function(measure) {
  assert_r6(measure, "Measure")
}

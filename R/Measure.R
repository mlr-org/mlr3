#' @title Base Class for Measures
#' @format [R6Class()] object
#'
#' @description
#' A [R6::R6Class()] to construct performance measures.
#' This is the abstract base class, do not use directly!
#'
#' Predefined measures are stored in [mlr_measures()].
#'
#' @field id (`character(1)`): Identifier of the measure.
#' @field description (`character(1)`): Description of the measure.
#' @field task.types (`character(1)`): Set of compatible task types.
#' @field fun (`function(truth, predicted)`): function to compute the measure.
#' @return [`Measure`].
#' @export
Measure = R6Class("Measure",
  public = list(
    id = NA_character_,
    description = NA_character_,
    task_types = NA_character_,
    packages = character(0L)
  )
)

assert_measures = function(measures) {
  assert_list(measures, types = "Measure")
}

as_measures = function(x, task) {
  if (is.null(x))
    return(list(mlr_measures$get(task$default_measure)))
  if (inherits(x, "Measure"))
    return(list(x))
  if (is.character(x))
    return(mlr_measures$mget(x))
  assert_measures(x)
}

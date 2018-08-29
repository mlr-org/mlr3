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
    id = NULL,
    description = NULL,
    task_types = character(0L),
    fun = NULL,
    aggregator = NULL,
    packages = NULL,
    initialize = function(id, description, task_types, fun, aggregator, packages = character(0L)) {
      self$id = assert_string(id, min.chars = 1L)
      self$description = assert_string(description, min.chars = 1L)
      self$task_types = assert_character(task_types, min.len = 1L, any.missing = FALSE)
      self$fun = assert_function(fun)
      self$aggregator = assert_function(aggregator)
      self$packages = assert_character(packages, any.missing = FALSE)
      environment(self$fun) = environment(self$initialize)
    }
  )
)

as_measures = function(x, task) {
  if (is.null(x))
    return(list(mlr_measures$get(task$default_measure)))
  if (inherits(x, "Measure"))
    return(list(x))
  if (is.character(x))
    return(mlr_measures$mget(x))
  assert_measures(x)
}

assert_measures = function(measures) {
  assert_list(measures, types = "Measure")
}

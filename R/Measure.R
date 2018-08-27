#' @title Base Class for Measures
#' @format \code{\link{R6Class}} object
#'
#' @description
#' A \code{\link[R6]{R6Class}} to construct performance measures.
#' This is the abstract base class, do not use directly!
#'
#' Predefined measures are stored in \code{\link{mlr.measures}}.
#'
#' @field id [\code{character(1)}]: Identifier of the measure.
#' @field description [\code{character(1)}]: Description of the measure.
#' @field task_types [\code{character}]: Set of compatible task_types.
#' @field fun [\code{function(truth, predicted)}]: function to compute the measure.
#' @return [\code{Measure}].
#' @export
Measure = R6Class("Measure",
  public = list(
    id = NULL,
    description = NULL,
    task_types = character(0L),
    fun = NULL,
    packages = NULL,
    initialize = function(id, description, task_types, fun, packages = character(0L)) {
      self$id = assert_string(id, min.chars = 1L)
      self$description = assert_string(description, min.chars = 1L)
      self$task_types = assert_character(task_types, min.len = 1L, any.missing = FALSE)
      self$fun = assert_function(fun)
      self$packages = assert_character(packages, any.missing = FALSE)
      environment(self$fun) = environment(self$initialize)
    }
  )
)

#' @include Dictionary.R
DictionaryMeasures = R6Class("DictionaryMeasures", inherit = Dictionary,
  public = list(
    initialize = function() {
      super$initialize("Measure")
    }
  )
)

#' @title Registered Measures
#' @docType class
#' @format \code{\link{R6Class}} object
#'
#' @description
#' \code{mlr.measures} is a \code{\link{Dictionary}} used to manage performance measures.
#'
#' @export
mlr.measures = DictionaryMeasures$new()

as_measures = function(x, task) {
  if (is.null(x))
    return(list(mlr.measures$get(task$default_measure)))
  if (inherits(x, "Measure"))
    return(list(x))
  if (is.character(x))
    return(mlr.measures$mget(x))
  assert_measures(x)
}

assert_measures = function(measures) {
  assert_list(measures, type = "Measure")
}

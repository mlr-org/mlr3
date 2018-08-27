#' @title Base Class for Resampling Measures
#' @format \code{\link{R6Class}} object
#'
#' @description
#' A \code{\link[R6]{R6Class}} to construct resampling measures.
#' This is the abstract base class, do not use directly!
#'
#' Predefined resampling measures are stored in \code{\link{mlr_resamplings}}.
#'
#' @field id [\code{character(1)}]: Identifier of the measure.
#' @field task_types [\code{character}]: Set of compatible task_types.
#' @field fun [\code{function(truth, predicted)}]: function to compute the measure.
#' @return [\code{Measure}].
#' @export
Resampling = R6Class("Resampling",
  active = list(
    is_instantiated = function() {
      !is.null(private$instance)
    },
    checksum = function() {
      if (is.null(private$instance))
        return(NA_character_)
      if (is.na(private$hash))
        private$hash = digest::digest(private$instance, algo = "murmur32")
      private$hash
    }
  ),

  private = list(
    instance = NULL,
    hash = NA_character_
  )
)

#' @include Dictionary.R
DictionaryResamplings = R6Class("DictionaryResamplings", inherit = Dictionary,
  public = list(
    initialize = function() {
      super$initialize("Resampling")
    }
  )
)

#' @title Registered Resampling Methods
#' @docType class
#' @format \code{\link{R6Class}} object
#'
#' @description
#' \code{Resamplings} is a \code{\link{Dictionary}} used to manage resampling methods.
#'
#' @export
mlr_resamplings = DictionaryResamplings$new()

assert_resampling = function(resampling) {
  assert_r6(resampling, "Resampling")
}

assert_resampling_index = function(r, i) {
  if (!r$is_instantiated)
    stopf("Resampling %s has not been instantiated yet", r$id)
  asInt(i, lower = 1L, upper = r$iters)
}

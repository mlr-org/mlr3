#' @title Base Class for Resampling Objects
#' @format \code{\link{R6Class}} object
#'
#' @description
#' A \code{\link[R6]{R6Class}} to construct resampling measures.
#' This is the abstract base class, do not use directly!
#'
#' Predefined resampling measures are stored in \code{\link{mlr_resamplings}}.
#'
#' @return [Resampling].
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


assert_resampling = function(resampling) {
  assert_r6(resampling, "Resampling")
}


assert_resampling_index = function(r, i) {
  if (!r$is_instantiated)
    stopf("Resampling %s has not been instantiated yet", r$id)
  asInt(i, lower = 1L, upper = r$iters)
}

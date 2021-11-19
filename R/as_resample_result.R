#' @title Convert to ResampleResult
#'
#' @description
#' Convert object to a [ResampleResult].
#'
#' @param x (any)\cr
#'  Object to convert.
#' @param ... (any)\cr
#'  Currently not used.
#'
#' @return ([ResampleResult]).
#' @export
as_resample_result = function(x, ...) {
  UseMethod("as_resample_result")
}

#' @rdname as_resample_result
#' @export
as_resample_result.ResampleResult = function(x, ...) { # nolint
  x
}

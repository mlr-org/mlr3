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

#' @rdname as_resample_result
#' @param view (`character()`)\cr
#'   See construction argument `view` of [`ResampleResult`].
#' @export
as_resample_result.ResultData = function(x, view = NULL) { # nolint
  ResampleResult$new(x, view = view)
}

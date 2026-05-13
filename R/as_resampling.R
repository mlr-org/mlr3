#' @title Convert to a Resampling
#'
#' @description
#' Convert object to a [Resampling] or a list of [Resampling].
#' This method e.g. allows to convert an `OMLTask` of \CRANpkg{mlr3oml} to a [`Resampling`].
#' @inheritParams as_task
#' @export
# nolint next
as_resampling = function(x, ...) {
  UseMethod("as_resampling")
}

#' @export
#' @rdname as_resampling
# nolint next
as_resampling.Resampling = function(x, clone = FALSE, ...) {
  assert_empty_ellipsis(...)
  if (isTRUE(clone)) x$clone() else x
}

#' @export
#' @rdname as_resampling
# nolint next
as_resamplings = function(x, ...) {
  UseMethod("as_resamplings")
}

#' @export
#' @rdname as_resampling
# nolint next
as_resamplings.default = function(x, ...) {
  list(as_resampling(x, ...))
}

#' @export
#' @rdname as_resampling
# nolint next
as_resamplings.list = function(x, ...) {
  lapply(x, as_resampling, ...)
}

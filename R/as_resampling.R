#' @title Convert to a Resampling
#'
#' @description
#' Convert object to a [Resampling] or a list of [Resampling].
#'
#' @inheritParams as_task
#' @export
as_resampling = function(x, ...) { # nolint
  UseMethod("as_resampling")
}

#' @export
#' @rdname as_resampling
as_resampling.Resampling = function(x, clone = FALSE, ...) { # nolint
  if (clone) x$clone() else x
}

#' @export
#' @rdname as_resampling
as_resamplings = function(x, ...) { # nolint
  UseMethod("as_resamplings")
}

#' @export
#' @rdname as_resampling
as_resamplings.list = function(x, clone = FALSE, ...) { # nolint
  lapply(x, as_resampling, clone = clone, ...)
}

#' @export
#' @rdname as_resampling
as_resamplings.Resampling = function(x, clone = FALSE, ...) { # nolint
  list(if (clone) x$clone() else x)
}

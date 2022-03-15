#' @title Convert to a Measure
#'
#' @description
#' Convert object to a [Measure] or a list of [Measure].
#'
#' @inheritParams as_task
#' @param task_type (`character(1)`)\cr
#'   Used if `x` is `NULL` to construct a default measure for the respective task type.
#'   The default measures are stored in [`mlr_reflections$default_measures`][mlr_reflections].
#'
#' @return [Measure].
#' @export
as_measure = function(x, ...) { # nolint
  if (is.null(x)) {
    return(default_measures(list(...)$task_type)[[1L]])
  }

  UseMethod("as_measure")
}

#' @export
#' @rdname as_measure
as_measure.Measure = function(x, clone = FALSE, ...) { # nolint
  if (clone) x$clone() else x
}

#' @export
#' @rdname as_measure
as_measures = function(x, ...) { # nolint
  if (is.null(x)) {
    return(default_measures(list(...)$task_type)[[1L]])
  }
  UseMethod("as_measures")
}

#' @export
#' @rdname as_measure
as_measures.list = function(x, clone = FALSE, ...) { # nolint
  lapply(x, as_measure, clone = clone, ...)
}

#' @export
#' @rdname as_measure
as_measures.Measure = function(x, clone = FALSE, ...) { # nolint
  list(if (clone) x$clone() else x)
}

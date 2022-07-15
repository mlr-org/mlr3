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
  UseMethod("as_measure")
}

#' @export
#' @rdname as_measure
as_measure.NULL = function(x, task_type = NULL, clone = FALSE, ...) { # nolint
  default_measures(task_type)[[1L]]
}

#' @export
#' @rdname as_measure
as_measure.character = function(x, ...) {
  msr(x, ...)
}

#' @export
#' @rdname as_measure
as_measure.Measure = function(x, clone = FALSE, ...) { # nolint
  if (clone) x$clone() else x
}

#' @export
#' @rdname as_measure
as_measures = function(x, ...) { # nolint
  UseMethod("as_measures")
}

#' @export
#' @rdname as_measure
as_measures.character = function(x, ...) {
  msrs(x, ...)
}

#' @export
#' @rdname as_measure
as_measures.NULL = function(x, task_type = NULL, clone = FALSE, ...) { # nolint
  default_measures(task_type)
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

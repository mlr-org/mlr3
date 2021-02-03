#' @title Object Coercion
#'
#' @name mlr_coercions
#' @description
#' S3 generics and methods to coerce to (lists of) [Task], [Learner], [Resampling], [Measure] and [Prediction].
#'
#' @param x (`any`)\cr
#'   Object to coerce.
#' @param clone (`logical(1)`)\cr
#'   If `TRUE`, ensures that the returned object is not the same as the input `x`, e.g.
#'   by cloning it or constructing it from a [dictionary][mlr3misc::Dictionary] such as [mlr_learners].
#' @param ... (`any`)\cr
#'   Additional arguments, currently ignored.
#'
#' @return Coerced object. The default method will return the object as-is.
#'   Failed coercions have to be handled by on of the assertions in [mlr_assertions].
#' @examples
#' # convert single measure to list of measures
#' measure = msr("classif.ce")
#' as_measures(measure)
NULL

#' @export
#' @rdname mlr_coercions
as_task = function(x, ...) {
  UseMethod("as_task")
}

#' @export
#' @rdname mlr_coercions
as_task.Task = function(x, clone = FALSE, ...) { # nolint
  if (clone) x$clone() else x
}

#' @export
#' @rdname mlr_coercions
as_tasks = function(x, ...) {
  UseMethod("as_tasks")
}

#' @export
#' @rdname mlr_coercions
as_tasks.list = function(x, clone = FALSE, ...) { # nolint
  lapply(x, as_task, clone = clone, ...)
}

#' @export
#' @rdname mlr_coercions
as_tasks.Task = function(x, clone = FALSE, ...) { # nolint
  list(if (clone) x$clone() else x)
}

#' @export
#' @rdname mlr_coercions
as_learner = function(x, ...) { # nolint
  UseMethod("as_learner")
}

#' @export
#' @rdname mlr_coercions
as_learner.Learner = function(x, clone = FALSE, ...) { # nolint
  if (clone) x$clone(deep = TRUE) else x
}

#' @export
#' @rdname mlr_coercions
as_learners = function(x, clone = FALSE, ...) { # nolint
  UseMethod("as_learners")
}

#' @export
#' @rdname mlr_coercions
as_learners.list = function(x, clone = FALSE, ...) { # nolint
  lapply(x, as_learner, clone = clone, ...)
}

#' @export
#' @rdname mlr_coercions
as_learners.Learner = function(x, clone = FALSE, ...) { # nolint
  list(if (clone) x$clone(deep = TRUE) else x)
}

#' @export
#' @rdname mlr_coercions
as_resampling = function(x, ...) { # nolint
  UseMethod("as_resampling")
}

#' @export
#' @rdname mlr_coercions
as_resampling.Resampling = function(x, clone = FALSE, ...) { # nolint
  if (clone) x$clone() else x
}

#' @export
#' @rdname mlr_coercions
as_resamplings = function(x, ...) { # nolint
  UseMethod("as_resamplings")
}

#' @export
#' @rdname mlr_coercions
as_resamplings.list = function(x, clone = FALSE, ...) { # nolint
  lapply(x, as_resampling, clone = clone, ...)
}

#' @export
#' @rdname mlr_coercions
as_resamplings.Resampling = function(x, clone = FALSE, ...) { # nolint
  list(if (clone) x$clone() else x)
}

#' @export
#' @rdname mlr_coercions
as_measure = function(x, ...) { # nolint
  UseMethod("as_measure")
}

#' @param task_type (`character(1)`)\cr
#'   Used if `x` is `NULL` to construct a default measure for the respective task type.
#'   The default measures are stored in [`mlr_reflections$default_measures`][mlr_reflections].
#' @export
#' @rdname mlr_coercions
as_measure.NULL = function(x, task_type = NULL, clone = FALSE, ...) { # nolint
  default_measures(task_type)[[1L]]
}

#' @export
#' @rdname mlr_coercions
as_measure.Measure = function(x, clone = FALSE, ...) { # nolint
  if (clone) x$clone() else x
}

#' @export
#' @rdname mlr_coercions
as_measures = function(x, ...) { # nolint
  UseMethod("as_measures")
}

#' @export
#' @rdname mlr_coercions
as_measures.NULL = function(x, task_type = NULL, clone = FALSE, ...) { # nolint
  default_measures(task_type)
}

#' @export
#' @rdname mlr_coercions
as_measures.list = function(x, clone = FALSE, ...) { # nolint
  lapply(x, as_measure, clone = clone, ...)
}

#' @export
#' @rdname mlr_coercions
as_measures.Measure = function(x, clone = FALSE, ...) { # nolint
  list(if (clone) x$clone() else x)
}

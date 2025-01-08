#' @title Convert to a Task
#'
#' @description
#' Convert object to a [Task] or a list of [Task].
#' This method e.g. allows to convert an [`mlr3oml::OMLTask`] to a [`Task`] and additionally supports cloning.
#' In order to construct a [Task] from a `data.frame`, use task-specific converters such as [`as_task_classif()`] or [`as_task_regr()`].
#'
#' @param x (any)\cr
#'   Object to convert.
#' @param ... (any)\cr
#'   Additional arguments.
#' @export
as_task = function(x, ...) {
  UseMethod("as_task")
}

#' @export
as_task.default = function(x, ...) {
  stopf("No method for class '%s'. To create a task from a `data.frame`, use dedicated converters such as `as_task_classif()` or `as_task_regr()`.", class(x)[1L])
}

#' @rdname as_task
#' @param clone (`logical(1)`)\cr
#'   If `TRUE`, ensures that the returned object is not the same as the input `x`.
#' @export
as_task.Task = function(x, clone = FALSE, ...) { # nolint
  assert_empty_ellipsis(...)
  if (isTRUE(clone)) x$clone(deep = TRUE) else x
}

#' @rdname as_task
#' @export
as_tasks = function(x, ...) {
  UseMethod("as_tasks")
}

#' @rdname as_task
#' @export
as_tasks.default = function(x, ...) { # nolint
  list(as_task(x, ...))
}

#' @rdname as_task
#' @export
as_tasks.list = function(x, ...) { # nolint
  lapply(x, as_task, ...)
}

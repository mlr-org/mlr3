#' @title Convert to a Task
#'
#' @description
#' Convert object to a [Task] or a list of [Task].
#'
#' @param x (`any`)\cr
#'   Object to convert.
#' @param ... (`any`)\cr
#'   Additional arguments.
#' @param clone (`logical(1)`)\cr
#'   If `TRUE`, ensures that the returned object is not the same as the input `x`.
#' @export
as_task = function(x, ...) {
  UseMethod("as_task")
}

#' @export
#' @rdname as_task
as_task.Task = function(x, clone = FALSE, ...) { # nolint
  if (clone) x$clone() else x
}

#' @export
#' @rdname as_task
as_tasks = function(x, ...) {
  UseMethod("as_tasks")
}

#' @export
#' @rdname as_task
as_tasks.list = function(x, clone = FALSE, ...) { # nolint
  lapply(x, as_task, clone = clone, ...)
}

#' @export
#' @rdname as_task
as_tasks.Task = function(x, clone = FALSE, ...) { # nolint
  list(if (clone) x$clone() else x)
}

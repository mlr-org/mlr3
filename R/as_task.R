#' @title Convert to a Task
#'
#' @description
#' Convert object to a [Task] or a list of [Task].
#'
#' @param x (any)\cr
#'   Object to convert.
#' @param ... (any)\cr
#'   Additional arguments.
#' @export
as_task = function(x, ...) {
  UseMethod("as_task")
}

#' @rdname as_task
#' @param clone (`logical(1)`)\cr
#'   If `TRUE`, ensures that the returned object is not the same as the input `x`.
#' @export
as_task.Task = function(x, clone = FALSE, ...) { # nolint
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

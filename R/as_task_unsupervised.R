#' @title Convert to an Unsupervised Task
#'
#' @description
#' Convert object to a [TaskUnsupervised] or a list of [TaskUnsupervised].
#'
#' @inheritParams as_task
#'
#' @export
as_task_unsupervised = function(x, ...) {
  UseMethod("as_task_unsupervised")
}

#' @rdname as_task_unsupervised
#' @export
as_task_unsupervised.Task = function(x, clone = FALSE, ...) { # nolint
  if (clone) x$clone() else x
}


#' @rdname as_task_unsupervised
#' @param id (`character(1)`)\cr
#'   Id for the new task.
#'   Defaults to the (deparsed and substituted) name of the data argument.
#' @template param_label
#' @export
as_task_unsupervised.data.frame = function(x, id = deparse(substitute(x)), label = NA_character_, ...) { # nolint
  ii = which(map_lgl(keep(x, is.double), anyInfinite))
  if (length(ii)) {
    warningf("Detected columns with unsupported Inf values in data: %s", str_collapse(names(ii)))
  }

  TaskUnsupervised$new(id = id, backend = x, label = label)
}

#' @rdname as_task_unsupervised
#' @export
as_tasks_unsupervised = function(x, ...) {
  UseMethod("as_tasks")
}

#' @rdname as_task_unsupervised
#' @param clone (`logical(1)`)\cr
#'   If `TRUE`, ensures that the returned object is not the same as the input `x`.
#' @export
as_task_unsupervised.list = function(x, clone = FALSE, ...) { # nolint
  lapply(x, as_task, clone = clone, ...)
}

#' @rdname as_task_unsupervised
#' @export
as_task_unsupervised.Task = function(x, clone = FALSE, ...) { # nolint
  list(if (clone) x$clone() else x)
}

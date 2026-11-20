#' @title Convert to an Unsupervised Task
#'
#' @description
#' Convert object to a [TaskUnsupervised] or a list of [TaskUnsupervised].
#' A [TaskUnsupervised] is returned as-is, possibly cloned.
#' Other tasks are converted with [convert_task()], by default the target becomes a feature.
#'
#' @inheritParams as_task
#'
#' @export
as_task_unsupervised = function(x, ...) {
  UseMethod("as_task_unsupervised")
}

#' @rdname as_task_unsupervised
#' @export
# nolint next
as_task_unsupervised.TaskUnsupervised = function(x, clone = FALSE, ...) {
  if (clone) x$clone(deep = TRUE) else x
}

#' @rdname as_task_unsupervised
#' @inheritParams convert_task
#' @export
# nolint next
as_task_unsupervised.Task = function(x, drop_original_target = FALSE, drop_levels = TRUE, ...) {
  convert_task(
    intask = x,
    new_type = "unsupervised",
    drop_original_target = drop_original_target,
    drop_levels = drop_levels
  )
}

#' @rdname as_task_unsupervised
#' @param id (`character(1)`)\cr
#'   Id for the new task.
#'   Defaults to the (deparsed and substituted) name of the data argument.
#' @template param_label
#' @export
# nolint next
as_task_unsupervised.data.frame = function(x, id = deparse1(substitute(x)), label = NA_character_, ...) {
  force(id)

  ii = which(map_lgl(keep(x, is.double), anyInfinite))
  if (length(ii)) {
    warning_input("Detected columns with unsupported Inf values in data: %s", str_collapse(names(ii)))
  }

  TaskUnsupervised$new(id = id, backend = x, label = label)
}

#' @rdname as_task_unsupervised
#' @export
# nolint next
as_task_unsupervised.DataBackend = function(x, id = deparse1(substitute(x)), label = NA_character_, ...) {
  force(id)

  TaskUnsupervised$new(id = id, backend = x, label = label)
}

#' @rdname as_task_unsupervised
#' @export
as_tasks_unsupervised = function(x, ...) {
  UseMethod("as_tasks_unsupervised")
}

#' @rdname as_task_unsupervised
#' @param clone (`logical(1)`)\cr
#'   If `TRUE`, ensures that the returned object is not the same as the input `x`.
#' @export
# nolint next
as_tasks_unsupervised.list = function(x, clone = FALSE, ...) {
  lapply(x, as_task_unsupervised, clone = clone, ...)
}

#' @rdname as_task_unsupervised
#' @export
# nolint next
as_tasks_unsupervised.Task = function(x, ...) {
  list(as_task_unsupervised(x, ...))
}

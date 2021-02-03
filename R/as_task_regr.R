#' @title Convert to a Regression Task
#'
#' @description
#' Convert object to a [TaskRegr].
#' This is a S3 generic, specialized for at least the following objects:
#'
#' 1. [TaskRegr]: ensure the identity
#' 2. [data.frame()] and [DataBackend]: provides an alternative to the constructor of [TaskRegr].
#' 3. [TaskClassif]: Calls [convert_task()].
#'
#' @inheritParams as_task
#'
#' @return [TaskRegr].
#' @export
#' @examples
#' as_task_regr(datasets::mtcars, target = "mpg")
as_task_regr = function(x, ...) {
  UseMethod("as_task_regr")
}


#' @rdname as_task_regr
#' @export
as_task_regr.TaskRegr = function(x, clone = FALSE, ...) { # nolint
  if (clone) x$clone() else x
}


#' @rdname as_task_regr
#' @template param_target
#' @param id (`character(1)`)\cr
#'   Id for the new task.
#'   Defaults to the (deparsed and substituted) name of `x`.
#' @export
as_task_regr.data.frame = function(x, target, id = deparse(substitute(x)), ...) { # nolint
  TaskRegr$new(id = id, backend = x, target = target)
}


#' @rdname as_task_regr
#' @export
as_task_regr.DataBackend = function(x, target, id = deparse(substitute(x)), ...) { # nolint
  TaskRegr$new(id = id, backend = x, target = target)
}


#' @rdname as_task_regr
#' @inheritParams convert_task
#' @export
as_task_regr.TaskClassif = function(x, target = NULL, drop_original_target = FALSE, drop_levels = TRUE, ...) { # nolint
  convert_task(intask = x, target = target, new_type = "regr", drop_original_target = FALSE, drop_levels = TRUE)
}

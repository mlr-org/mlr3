#' @title Convert to a Regression Task
#'
#' @description
#' Convert object to a regression task ([TaskRegr]).
#'
#' @inheritParams as_task
#'
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

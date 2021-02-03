#' @title Convert to a Classification Task
#'
#' @description
#' Convert object to a classification task ([TaskClassif]).
#'
#' @inheritParams as_task
#'
#' @export
#' @examples
#' as_task_classif(palmerpenguins::penguins, target = "species")
as_task_classif = function(x, ...) {
  UseMethod("as_task_classif")
}


#' @rdname as_task_classif
#' @export
as_task_classif.TaskClassif = function(x, clone = FALSE, ...) { # nolint
  if (clone) x$clone() else x
}


#' @rdname as_task_classif
#' @template param_target
#' @param id (`character(1)`)\cr
#'   Id for the new task.
#'   Defaults to the (deparsed and substituted) name of `x`.
#' @param positive (`character(1)`)\cr
#'   Level of the positive class. See [TaskClassif].
#' @export
as_task_classif.data.frame = function(x, target = NULL, id = deparse(substitute(x)), positive = NULL, ...) { # nolint
  TaskClassif$new(id = id, backend = x, target = target, positive = positive)
}


#' @rdname as_task_classif
#' @export
as_task_classif.DataBackend = function(x, target = NULL, id = deparse(substitute(x)), positive = NULL, ...) { # nolint
  TaskClassif$new(id = id, backend = x, target = target, positive = positive)
}

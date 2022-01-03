#' @title Convert to a Classification Task
#'
#' @description
#' Convert object to a [TaskClassif].
#' This is a S3 generic, specialized for at least the following objects:
#'
#' 1. [TaskClassif]: ensure the identity
#' 2. [data.frame()] and [DataBackend]: provides an alternative to the constructor of [TaskClassif].
#' 3. [TaskRegr]: Calls [convert_task()].
#'
#' @inheritParams as_task
#'
#' @return [TaskClassif].
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
  ii = which(map_lgl(x[map_lgl(x, is.double)], anyInfinite))
  if (length(ii)) {
    warningf("Detected columns with unsupported Inf values in data: %s", str_collapse(names(ii)))
  }

  TaskClassif$new(id = id, backend = x, target = target, positive = positive)
}


#' @rdname as_task_classif
#' @export
as_task_classif.DataBackend = function(x, target = NULL, id = deparse(substitute(x)), positive = NULL, ...) { # nolint
  TaskClassif$new(id = id, backend = x, target = target, positive = positive)
}

#' @rdname as_task_classif
#' @inheritParams convert_task
#' @export
as_task_classif.TaskRegr = function(x, target = NULL, drop_original_target = FALSE, drop_levels = TRUE, ...) { # nolint
  convert_task(intask = x, target = target, new_type = "classif", drop_original_target = FALSE, drop_levels = TRUE)
}

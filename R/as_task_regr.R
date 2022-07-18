#' @title Convert to a Regression Task
#'
#' @description
#' Convert object to a [TaskRegr].
#' This is a S3 generic. mlr3 ships with methods for the following objects:
#'
#' 1. [TaskRegr]: ensure the identity
#' 2. [`formula`], [data.frame()], [matrix()], [Matrix::Matrix()] and [DataBackend]: provides an alternative to the constructor of [TaskRegr].
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
#'   Defaults to the (deparsed and substituted) name of the data argument.
#' @template param_label
#' @export
as_task_regr.data.frame = function(x, target, id = deparse(substitute(x)), label = NA_character_, ...) { # nolint
  assert_data_frame(x, min.rows = 1L, min.cols = 1L, col.names = "unique")
  ii = which(map_lgl(keep(x, is.double), anyInfinite))
  if (length(ii)) {
    warningf("Detected columns with unsupported Inf values in data: %s", str_collapse(names(ii)))
  }

  TaskRegr$new(id = id, backend = x, target = target, label = label)
}

#' @rdname as_task_regr
#' @export
as_task_regr.matrix = function(x, target, id = deparse(substitute(x)), label = NA_character_, ...) { # nolint
  assert_matrix(x, mode = "numeric")
  as_task_regr(as.data.table(x), target = target, id = id, label = label, ...)
}

#' @rdname as_task_regr
#' @export
as_task_regr.Matrix = function(x, target, id = deparse(substitute(x)), label = NA_character_, ...) { # nolint
  dense = data.table(..row_id = seq_len(nrow(x)))
  b = DataBackendMatrix$new(x, dense = dense, primary_key = "..row_id")
  as_task_regr(b, target = target, id = id, label = label, ...)
}

#' @rdname as_task_regr
#' @export
as_task_regr.DataBackend = function(x, target, id = deparse(substitute(x)), label = NA_character_, ...) { # nolint
  TaskRegr$new(id = id, backend = x, target = target, label = label, ...)
}

#' @rdname as_task_regr
#' @inheritParams convert_task
#' @export
as_task_regr.TaskClassif = function(x, target = NULL, drop_original_target = FALSE, drop_levels = TRUE, ...) { # nolint
  convert_task(intask = x, target = target, new_type = "regr", drop_original_target = FALSE, drop_levels = TRUE)
}

#' @rdname as_task_regr
#' @param data (`data.frame()`)\cr
#'   Data frame containing all columns referenced in formula `x`.
#' @export
as_task_regr.formula = function(x, data, id = deparse(substitute(data)), label = NA_character_, ...) { # nolint
  assert_data_frame(data)
  assert_subset(all.vars(x), c(names(data), "."), .var.name = "formula")
  if (!attributes(terms(x, data = data))$response) {
    stopf("Formula %s is missing a response", format(x))
  }
  tab = model.frame(x, data, na.action = "na.pass")
  attr(tab, "terms") = attr(tab, "na.action") = NULL
  target = all.vars(x)[1L]

  as_task_regr(tab, target = target, id = id, label = label, ...)
}

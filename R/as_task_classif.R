#' @title Convert to a Classification Task
#'
#' @description
#' Convert object to a [TaskClassif].
#' This is a S3 generic. mlr3 ships with methods for the following objects:
#'
#' 1. [TaskClassif]: ensure the identity
#' 2. [`formula`], [data.frame()], [matrix()], [Matrix::Matrix()] and [DataBackend]: provides an alternative to the constructor of [TaskClassif].
#' 3. [TaskRegr]: Calls [convert_task()].
#'
#' Note that the target column will be converted to a `factor()`, if possible.
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
#'   Defaults to the (deparsed and substituted) name of the data argument.
#' @param positive (`character(1)`)\cr
#'   Level of the positive class. See [TaskClassif].
#' @template param_label
#' @export
as_task_classif.data.frame = function(x, target = NULL, id = deparse(substitute(x)), positive = NULL, label = NA_character_, ...) { # nolint
  expect_data_frame(x, min.rows = 1L, min.cols = 1L, names = "unique")
  ii = which(map_lgl(keep(x, is.double), anyInfinite))
  if (length(ii)) {
    warningf("Detected columns with unsupported Inf values in data: %s", str_collapse(names(ii)))
  }

  y = x[[target]]
  if (!is.factor(y)) {
    x[[target]] = as.factor(y)
  }

  TaskClassif$new(id = id, backend = x, target = target, positive = positive, label = label)
}

#' @rdname as_task_classif
#' @export
as_task_classif.matrix = function(x, target, id = deparse(substitute(x)), label = NA_character_, ...) { # nolint
  expect_matrix(x, col.names = "unique", min.rows = 1L, min.cols = 1L)
  as_task_classif(as.data.table(x), target = target, id = id, label = label, ...)
}

#' @rdname as_task_classif
#' @export
as_task_classif.Matrix = function(x, target, id = deparse(substitute(x)), label = NA_character_, ...) { # nolint
  assert_names(colnames(x), "unique")
  assert_choice(target, colnames(x))
  ii = which(target == colnames(x))
  dense = data.table(..row_id = seq_len(nrow(x)))
  y = x[, target, drop = TRUE]

  if (!is.factor(y)) {
    # move target col from matrix to data frame for conversion
    set(dense, j = target, value = factor(x[, ii, drop = TRUE]))
    x = x[, -ii, drop = FALSE]
  }

  b = DataBackendMatrix$new(x, dense = dense, primary_key = "..row_id")
  as_task_classif(b, target = target, id = id, label = label, ...)
}

#' @rdname as_task_classif
#' @export
as_task_classif.DataBackend = function(x, target = NULL, id = deparse(substitute(x)), positive = NULL, label = NA_character_, ...) { # nolint
  TaskClassif$new(id = id, backend = x, target = target, positive = positive, label = label, ...)
}

#' @rdname as_task_classif
#' @inheritParams convert_task
#' @export
as_task_classif.TaskRegr = function(x, target = NULL, drop_original_target = FALSE, drop_levels = TRUE, ...) { # nolint
  convert_task(intask = x, target = target, new_type = "classif", drop_original_target = FALSE, drop_levels = TRUE)
}

#' @rdname as_task_classif
#' @param data (`data.frame()`)\cr
#'   Data frame containing all columns referenced in formula `x`.
#' @export
as_task_classif.formula = function(x, data, id = deparse(substitute(data)), positive = NULL, label = NA_character_, ...) { # nolint
  assert_data_frame(data)
  assert_subset(all.vars(x), c(names(data), "."), .var.name = "formula")
  if (!attributes(terms(x, data = data))$response) {
    stopf("Formula %s is missing a response", format(x))
  }
  tab = model.frame(x, data, na.action = "na.pass")
  attr(tab, "terms") = attr(tab, "na.action") = NULL
  target = all.vars(x)[1L]

  as_task_classif(tab, target = target, id = id, positive = positive, label = label, ...)
}

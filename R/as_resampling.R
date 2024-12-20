#' @title Convert to a Resampling
#'
#' @description
#' Convert object to a [Resampling] or a list of [Resampling].
#'
#' For the conversion of a `data.table`, the following columns (as returned by
#' [as.data.table.Resampling()]) are mandatory to create a [ResamplingCustom]:
#' * `set`: factor with levels `"train"` and `"test"`
#' * `iteration`: non-negative integer
#' * `row_id`: integer row ids
#'
#' @inheritParams as_task
#' @export
as_resampling = function(x, ...) { # nolint
  UseMethod("as_resampling")
}

#' @export
#' @rdname as_resampling
as_resampling.Resampling = function(x, clone = FALSE, ...) { # nolint
  if (isTRUE(clone)) x$clone() else x
}


#' @export
#' @rdname as_resampling
as_resampling.data.table = function(x, ...) { # nolint
  assert_data_table(x, min.rows = 1L, col.names = "unique")
  assert_names(names(x), permutation.of = c("set", "iteration", "row_id"))
  assert_factor(x$set, any.missing = FALSE)
  assert_set_equal(levels(x$set), c("train", "test"))
  assert_integerish(x$iteration, lower = 1L, any.missing = FALSE)
  assert_integerish(x$row_id, any.missing = FALSE)

  row_id = NULL
  resampling = ResamplingCustom$new()
  resampling$instance = list(
    train_sets = x[list("train"), list(ids = list(row_id)), by = "iteration"]$ids,
    test_sets = x[list("test"), list(ids = list(row_id)), by = "iteration"]$ids
  )
  resampling
}

#' @export
#' @rdname as_resampling
as_resamplings = function(x, ...) { # nolint
  UseMethod("as_resamplings")
}

#' @export
#' @rdname as_resampling
as_resamplings.default = function(x, ...) { # nolint
  list(as_resampling(x, ...))
}

#' @export
#' @rdname as_resampling
as_resamplings.list = function(x, ...) { # nolint
  lapply(x, as_resampling, ...)
}

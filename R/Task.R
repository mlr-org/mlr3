#' @title Abstract learning task
#'
#' @description
#' This is the abstract base class for task objects like [TaskClassif] and [TaskRegr].
#'
#' @section Usage:
#' ```
#' t = Task$new(id, backend)
#'
#' t$id
#' t$backend
#' t$row_info
#' t$col_info
#' t$measures
#' t$data(rows = NULL, cols = NULL)
#' t$head(n = 6)
#' t$row_ids(subset = NULL)
#' t$features_names
#' t$target_names
#' t$nrow
#' t$ncol
#' t$col_types
#' t$formula
#' ```
#'
#' @section Arguments:
#' * `id` (`string`):
#'   Name of the task.
#' * `backend` ([Backend]):
#'   [Backend] which stores the data.
#' * `rows` (`vector`):
#'   Vector of row ids used to subset rows from the [Backend] using its primary key.
#'   Can be `character()` or `integer`, depending on the [Backend].
#' * `cols` (`character()`):
#'   Character vector of used to select columns from the [Backend].
#' * `n` (`integer(1)`):
#'   Number of rows to retrieve from the [Backend].
#' * `subset` (`vector`):
#'   Subset of row ids to subset rows from the [Backend] using its primary key.
#'
#' @section Details:
#' `$new()` initializes a new object of class [Task].
#'
#' `$id` (`character(1)`) stores the name of the task.
#'
#' `$backend()` ([Backend]) stores the [Backend] of the task.
#'
#' `$row_info` (`data.table`) with columns `id` and `role`.
#' Stores row ids of [Backend] in column `id`. Each row (observation)
#' can have a specific mutually exclusive role in the learning task:
#' - `"use"`: Use in training.
#' - `"validation"`: Do not use in training, this are (possibly unlabeled) observations
#'   which are held back unless explicitly addressed.
#' - `"ignore"`: Do not these observations at all.
#'
#' `$col_info` (`data.table`) with columns `id`, `role` and `type`.
#' Stores column names of [Backend] in column `id`. Each column (feature)
#' can have a specific mutually exclusive role in the learning task:
#' - `"feature"`: Regular feature.
#' - `"target"`: Column with target labels.
#' - `"ignore"`: Do not these features at all.
#' - `"primary_key"`: Name of the primary id column used in [Backend].
#' Column `type` stores the storage type of the variable, e.g. `integer`, `numeric` or `character`.
#'
#' `measures` is a list of [Measure] (performance measures) to use in this task.
#'
#' `data()` is used to retrieve data from the backend as `data.table`.
#' Rows are subsetted to only contain observations with `role == "use"`.
#' Columns are filtered to only contain features with `role %in% c("target", "feature")`.
#' If invalid `rows` or `cols` are specified, an exception is raised.
#'
#' `head()` can be used to peek into the first `n` observations with `role == "use"`.
#'
#' `row_ids()` returns a (subset of) row ids used in the task, i.e. subsetted to observations with `role == "use"`.
#'
#' `feature_names` returns a `character` vector of all feature names with `role == "feature"`.
#'
#' `target_names` returns a `character` vector of all feature names with `role == "target"`.
#'
#' `nrow` provides the total number of rows with `role == "use"`.
#'
#' `ncol` provides the total number of cols with `role %in% c("target", "feature")`.
#'
#' `col_types` gives a `data.table` with columns `id` and `type` where `id` are the column names of "active" columns of the task and `type` is the storage type.
#'
#' `formula` constructs a [stats::formula], e.g. `[target] ~ [feature_1] + [feature_2] + ... + [feature_k]`.
#'
#' @name Task
#' @export
#' @family Tasks
#' @keywords internal
#' @examples
#' b = BackendDataTable$new(iris)
#' task = Task$new("iris", b)
#' task$nrow
#' task$ncol
#' task$head()
#' task$formula
#'
#' # Ignore "Petal.Length"
#' task$col_info[id == "Petal.Length", role := "ignore"]
#' task$formula
NULL

#' @include capabilities.R
Task = R6Class("Task",
  cloneable = TRUE,
  public = list(
    id = NULL,
    backend = NULL,
    row_info = NULL,
    col_info = NULL,
    measures = list(),
    order = character(0L),

    initialize = function(id, backend) {
      self$id = assert_id(id)
      self$backend = assert_backend(backend)
      self$row_info = data.table(id = backend$rownames, role = "use", key = "id")
      self$col_info = col_types(backend$head(1L))[, "role" := "feature"]
      self$col_info[id == backend$primary_key, role := "primary_key"]
    },

    print = function(...) {
      task_print(self)
    },

    data = function(rows = NULL, cols = NULL) {
      task_data(self, rows, cols)
    },

    head = function(n = 6L) {
      assert_count(n)
      ids = head(self$row_info[role == "use", "id", with = FALSE][[1L]], n)
      self$data(rows = ids, cols = c(self$feature_names, self$target_names))
    },

    row_ids = function(subset = NULL) {
      if (is.null(subset)) {
        self$row_info[role == "use", "id"][[1L]]
      } else {
        self$row_info[list(subset)][role == "use", "id"][[1L]]
      }
    },

    filter = function(row_ids) {
      self$row_info[!(id %in% row_ids) & role == "use", role := "ignore"]
      self
    },

    select = function(cols) {
      self$col_info[!(id %in% cols) & role == "feature", role := "ignore"]
      self
    },

    rbind = function(data) {
      self$backend = backend_rbind(self$backend, data)
      extra_info = data.table(id = data[[self$backend$primary_key]], role = "use")
      self$row_info = rbind(self$row_info, extra_info)
      setkeyv(self$row_info, "id")
      self
    },

    cbind = function(data) {
      self$backend = backend_cbind(self$backend, data)
      extra_info = col_types(data)[, "role" := "feature"]
      self$col_info = rbind(self$col_info, extra_info[!(self$backend$primary_key)])
      setkeyv(self$col_info, "id")
      self
    }
  ),

  active = list(
    feature_names = function() {
      self$col_info[role == "feature", "id"][[1L]]
    },

    target_names = function() {
      self$col_info[role == "target", "id"][[1L]]
    },

    nrow = function() {
      self$row_info[role == "use", .N]
    },

    ncol = function() {
      self$col_info[role %in% c("feature", "target"), .N]
    },

    col_types = function() {
      self$col_info[role %in% c("feature", "target"), c("id", "type")]
    },

    formula = function() {
      tn = self$target_names
      if (length(tn) == 0L)
        tn = NULL
      f = reformulate(self$feature_names, response = tn)
      environment(f) = NULL
      f
    }
  ),

  private = list(
    deep_clone = function(name, value) {
      # NB: Backends are never copied!
      if (name %in% c("row_info", "col_info")) copy(value) else value
    }
  )
)

task_data = function(self, rows = NULL, cols = NULL) {
  if (is.null(rows)) {
    selected_rows = self$row_info[role == "use", "id"][[1L]]
  } else {
    if (self$row_info[list(rows), .N] != length(rows))
      stopf("Invalid row ids provided")
    selected_rows = rows
  }

  if (is.null(cols)) {
    selected_cols = self$col_info[role %in% c("feature", "target"), "id"][[1L]]
  } else {
    selected_cols = self$col_info[id %in% cols & role %in% c("feature", "target"), "id"][[1L]]
    if (length(selected_cols) != length(cols))
      stopf("Invalid column ids provided")
  }

  extra_cols = character(0L)
  if (length(self$order)) {
    extra_cols = setdiff(self$order, selected_cols)
    selected_cols = union(selected_cols, extra_cols)
  }

  data = self$backend$data(rows = selected_rows, cols = selected_cols)

  if (nrow(data) != length(selected_rows)) {
    stopf("Backend did not return the rows correctly: %i requested, %i received", length(selected_rows), nrow(data))
  }

  if (ncol(data) != length(selected_cols)) {
    stopf("Backend did not return the cols correctly: %i requested, %i received", length(selected_cols), ncol(data))
  }

  if (length(self$order)) {
    setorderv(data, self$order)
  }

  if (length(extra_cols))
    data[, (extra_cols) := NULL]
  return(data)
}

task_print = function(self) {
  catf("Task '%s' of type %s (%i x %i)", self$id, class(self)[1L], self$nrow, self$ncol)
  catf(stri_list("Target: ", self$target_names))
  catf(stri_list("Features: ", stri_peek(self$feature_names)))
  catf(stri_list("Order by: ", self$order))
  catf(stri_list("Public: ", setdiff(ls(self), c("initialize", "print"))))
}

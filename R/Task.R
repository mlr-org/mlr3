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
#' t$set_row_role(rows, new_role)
#' t$set_col_role(cols, new_role)
#' t$measures
#' t$data(rows = NULL, cols = NULL)
#' t$head(n = 6)
#' t$levels(col)
#' t$row_ids(subset = NULL)
#' t$features_names
#' t$target_names
#' t$nrow
#' t$ncol
#' t$col_types
#' t$formula
#' t$hash
#'
#' t$filter(rows)
#' t$select(cols)
#' t$rbind(data)
#' t$cbind(data)
#' ```
#'
#' @section Arguments:
#' * `id` (`string`):
#'   Name of the task.
#' * `backend` ([Backend]):
#'   [Backend] which stores the data.
#' * `data` ([base::data.frame]):
#'   New data to rbind/cbind to the task.
#' * `rows` (`vector`):
#'   Vector of row ids specifying rows from the [Backend] using its primary key.
#'   Can be `character()` or `integer`, depending on the [Backend].
#' * `cols` (`character()`):
#'   Character vector to specify columns from the [Backend].
#' * `col` (`character(1)`):
#'   Character vector to specify a single column from the [Backend].
#' * `n` (`integer(1)`):
#'   Number of rows to retrieve from the [Backend].
#' * `new_role` (`character(1)`):
#'   New role to assign for specified rows/columns.
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
#' To alter the role, use `set_row_role()`.
#'
#' `$col_info` (`data.table`) with columns `id`, `role` and `type`.
#'   Stores column names of [Backend] in column `id`. Each column (feature)
#'   can have a specific mutually exclusive role in the learning task:
#'   - `"feature"`: Regular feature.
#'   - `"target"`: Column with target labels.
#'   - `"ignore"`: Do not these features at all.
#'   - `"primary_key"`: Name of the primary id column used in [Backend].
#'   Column `type` stores the storage type of the variable, e.g. `integer`, `numeric` or `character`.
#' To alter the role, use `set_col_role()`.
#'
#' `$set_row_role()` overwrites the role for specified rows, referenced by row id.
#'
#' `$set_col_role()` overwrites the role for specified columns.
#'
#' `$measures` is a list of [Measure] (performance measures) to use in this task.
#'
#' `$data()` is used to retrieve data from the backend as `data.table`.
#'   Rows are subsetted to only contain observations with `role == "use"`.
#'   Columns are filtered to only contain features with `role %in% c("target", "feature")`.
#'   If invalid `rows` or `cols` are specified, an exception is raised.
#'
#' `$head()` can be used to peek into the first `n` observations with `role == "use"`.
#'
#' `$levels()` queries the distinct levels of the column `col`. Only works for `character` and `factor` columns.
#'   This function ignores the row roles, so that you get all levels found in the [Backend].
#'
#' `$row_ids()` returns a (subset of) row ids used in the task, i.e. subsetted to observations with `role == "use"`.
#'
#' `$feature_names` returns a `character` vector of all feature names with `role == "feature"`.
#'
#' `$target_names` returns a `character` vector of all feature names with `role == "target"`.
#'
#' `$nrow` provides the total number of rows with `role == "use"`.
#'
#' `$ncol` provides the total number of cols with `role %in% c("target", "feature")`.
#'
#' `$col_types` gives a `data.table` with columns `id` and `type` where `id` are the column names of "active" columns of the task and `type` is the storage type.
#'
#' `$formula` constructs a [stats::formula], e.g. `[target] ~ [feature_1] + [feature_2] + ... + [feature_k]`.
#'
#' `$filter()` reduces the task, subsetting it to only the rows specified.
#'
#' `$select()` reduces the task, subsetting it to only the columns specified.
#'
#' `$rbind()` extends the task with additional rows.
#'
#' `$cbind()` extends the task with additional columns.
#'
#' `$hash` stores a checksum (`character(1)`) calculated on the `id`, `row_info` and `col_info`.
#'
#' @name Task
#' @export
#' @family Task
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
    type = NA_character_,
    backend = NULL,
    row_info = NULL,
    col_info = NULL,
    measures = list(),
    order = character(0L),

    initialize = function(id, backend) {
      self$id = assert_id(id)
      self$backend = assert_backend(backend)
      self$row_info = data.table(id = backend$rownames, role = "use", key = "id")
      self$col_info = col_info(backend, self$backend$primary_key)
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

    levels = function(col) {
      assert_choice(col, self$col_info$id)
      self$col_info[list(col), "levels", with = FALSE][[1L]][[1L]]
    },

    row_ids = function(subset = NULL) {
      if (is.null(subset)) {
        self$row_info[role == "use", "id"][[1L]]
      } else {
        self$row_info[list(subset)][role == "use", "id"][[1L]]
      }
    },

    filter = function(rows) {
      self$row_info[!(id %in% rows) & role == "use", role := "ignore"]
      self
    },

    select = function(cols) {
      self$col_info[!(id %in% cols) & role == "feature", role := "ignore"]
      self
    },

    rbind = function(data) {
      task_rbind(self, data)
    },

    cbind = function(data) {
      task_cbind(self, data)
    },

    set_row_role = function(rows, new_role) {
      # TODO: Make this an active binding?
      assert_choice(new_role, capabilities$task_row_roles)
      self$row_info[list(rows), "role" := new_role]
      private$.hash = NA_character_
      self
    },

    set_col_role = function(cols, new_role) {
      # TODO: Make this an active binding?
      assert_choice(new_role, capabilities$task_col_roles)
      self$col_info[list(cols), "role" := new_role]
      private$.hash = NA_character_
      self
    }
  ),

  active = list(
    hash = function() {
      if (is.na(private$.hash))
        private$.hash = digest::digest(list(self$id, self$row_info, self$col_info), algo = "xxhash64")
      private$.hash
    },

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
    .hash = NA_character_,
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


# Performs the following steps to virtually rbind data to the task:
# 1. Check that an rbind is feasible
# 2. Update row_info
# 3. Update col_info
# 4. Overwrite self$backend with new backend
task_rbind = function(self, data) {
  # 1. Check that an rbind is feasible
  assert_data_frame(data, min.rows = 1L, min.cols = 1L)
  data = as.data.table(data)
  pk = self$backend$primary_key
  auto_incremented = FALSE

  ## 1.1 Check for primary key column and auto-increment if possible
  if (pk %nin% names(data)) {
    rids = self$row_ids()
    if (is.integer(rids)) {
      data[[pk]] = max(rids) + seq_row(data)
      auto_incremented = TRUE
    } else {
      stopf("Cannot rbind to task: Missing primary key column '%s'", pk)
    }
  } else {
    assert_atomic_vector(data[[pk]], any.missing = FALSE, unique = TRUE)
  }

  ## 1.2 Check for set equality of column names
  assert_set_equal(self$col_info$id, names(data))

  ## 1.3 Check that there are now duplicated row_ids
  if (!auto_incremented) {
    tmp = self$backend$data(data[[pk]], pk)[[1L]]
    if (length(tmp))
      stopf("Cannot rbind task: Duplicated row ids: %s", stri_peek(tmp))
  }

  ## 1.4 Check that types are matching
  data_col_info = col_info(data, pk)
  tmp = head(merge(self$col_info, data_col_info, by = "id")[get("type.x") != get("type.y")], 1L)
  if (nrow(tmp)) {
    stopf("Cannot rbind task: Types do not match for column: %s (%s != %s)", stri_peek(tmp$id), tmp$type.x, tmp$type.y)
  }

  # 2. Update row_info
  self$row_info = setkeyv(rbindlist(list(self$row_info, data.table(id = data[[pk]], role = "use"))), "id")

  # 3. Update col_info
  self$col_info$levels = Map(union, self$col_info$levels, data_col_info$levels)

  # 4. Overwrite self$backend with new backend
  self$backend = BackendRbind$new(self$backend, BackendDataTable$new(data, primary_key = pk))
}

# Performs the following steps to virtually cbind data to the task:
# 1. Check that an cbind is feasible
# 2. Update col_info
# 3. Overwrite self$backend with new backend
task_cbind = function(self, data) {
  # 1. Check that an cbind is feasible
  assert_data_frame(data, min.rows = 1L, min.cols = 2L)
  data = as.data.table(data)
  pk = self$backend$primary_key

  ## 1.1 Check for primary key column
  if (pk %nin% names(data)) {
    stopf("Cannot cbind task: Missing primary key column '%s'", self$backend$primary_key)
  }

  if (self$col_info[role == "primary_key", "type"][[1L]] != class(data[[pk]])) {
    stopf("Cannot cbind task: Primary key column '%s' has wrong type", self$backend$primary_key)
  }

  ## 1.2 Check that there are no duplicated column names
  tmp = setdiff(intersect(self$col_info$id, names(data)), pk)
  if (length(tmp)) {
    stopf("Cannot cbind task: Duplicated column names: %s", stri_peek(tmp))
  }

  ## 1.3 Check for set equality of row ids
  assert_atomic_vector(data[[pk]], any.missing = FALSE, unique = TRUE)
  if (self$backend$data(data[[pk]], pk)[, .N] != nrow(data)) {
    stopf("Cannot cbind task: Row ids do not match")
  }

  # 2. Update col_info
  data_col_info = col_info(data, pk)
  self$col_info = setkeyv(rbindlist(list(self$col_info, data_col_info[!list(pk)])), "id")

  # 3. Overwrite self$backend with new backend
  self$backend = BackendCbind$new(self$backend, BackendDataTable$new(data, primary_key = pk))
}

task_print = function(self) {
  catf("Task '%s' of type %s (%i x %i)", self$id, class(self)[1L], self$nrow, self$ncol)
  catf(stri_list("Target: ", self$target_names))
  catf(stri_list("Features: ", stri_peek(self$feature_names)))
  catf(stri_list("Order by: ", self$order))
  catf(stri_list("Public: ", setdiff(ls(self), c("initialize", "print"))))
}


col_info = function(x, primary_key = NULL) {
  is_backend = inherits(x, "Backend")
  types = vcapply(if (is_backend) x$head(1L) else x, class)
  col_info = data.table(id = names(types), type = unname(types), role = "feature", levels = list(), key = "id")

  if (!is.null(primary_key))
    col_info[list(primary_key), role := "primary_key"]

  discrete = col_info[get("type") %in% c("character", "factor"), "id"][[1L]]
  if (length(discrete)) {
    discrete = if (is_backend) x$distinct(discrete) else lapply(x[, discrete, with = FALSE], distinct)
    col_info[list(names(discrete)), levels := list(discrete)]
  }

  col_info[]
}

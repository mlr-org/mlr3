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
#' t$row_roles
#' t$col_info
#' t$set_row_role(rows, new_roles, exclusive = TRUE)
#' t$set_col_role(cols, new_roles, exclusive = TRUE)
#' t$measures
#' t$data(rows = NULL, cols = NULL)
#' t$head(n = 6)
#' t$levels(col)
#' t$row_ids(subset = NULL)
#' t$features_names
#' t$target_names
#' t$nrow
#' t$ncol
#' t$feature_types
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
#' * `backend` ([DataBackend]):
#'   [DataBackend] which stores the data.
#' * `data` ([base::data.frame]):
#'   New data to rbind/cbind to the task.
#' * `rows` (`vector`):
#'   Vector of row ids specifying rows from the [DataBackend] using its primary key.
#'   Can be `character()` or `integer`, depending on the [DataBackend].
#' * `cols` (`character()`):
#'   Character vector to specify columns from the [DataBackend].
#' * `col` (`character(1)`):
#'   Character vector to specify a single column from the [DataBackend].
#' * `n` (`integer(1)`):
#'   Number of rows to retrieve from the [DataBackend].
#' * `new_roles` (`character(1)`):
#'   New roles to assign for specified rows/columns.
#' * `exclusive` (`logical(1)`):
#'   If `TRUE`, the cols/rows will be removed from all roles except `new_roles`.
#' * `subset` (`vector`):
#'   Subset of row ids to subset rows from the [DataBackend] using its primary key.
#'
#' @section Details:
#' `$new()` initializes a new object of class [Task].
#'
#' `$id` (`character(1)`) stores the name of the task.
#'
#' `$backend()` ([DataBackend]) stores the [DataBackend] of the task.
#'
#' `$row_roles` (`list`).
#' Stores the row ids of [DataBackend] in vectors of row roles:
#' - `"use"`: Use in training.
#' - `"validation"`: Do not use in training, this are (possibly unlabeled) observations
#'   which are held back unless explicitly addressed.
#' To alter the role, use `set_row_role()`.
#'
#' `$col_info` (`data.table`) with columns `id`, `role` and `type`.
#'   Stores column names of [DataBackend] in column `id`. Each column (feature)
#'   can have a specific mutually exclusive role in the learning task:
#'   - `"feature"`: Regular feature.
#'   - `"target"`: Column with target labels.
#'   - `"ignore"`: Do not these features at all.
#'   - `"primary_key"`: Name of the primary id column used in [DataBackend].
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
#'   This function ignores the row roles, so you get all levels found in the [DataBackend].
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
#' `$feature_types` gives a `data.table` with columns `id` and `type` where `id` are the column names of "active" features of the task and `type` is the storage type.
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
#' `$hash` stores a checksum (`character(1)`) calculated on the `id`, `row_roles` and `col_info`.
#'
#' @name Task
#' @export
#' @family Task
#' @keywords internal
#' @examples
#' b = DataBackendDataTable$new(iris)
#' task = Task$new("iris", b)
#' task$nrow
#' task$ncol
#' task$head()
#' task$formula
#'
#' # Remove "Petal.Length"
#' task$set_col_role("Petal.Length", character(0L))
#' task$formula
NULL

#' @include capabilities.R
Task = R6Class("Task",
  cloneable = TRUE,
  public = list(
    id = NULL,
    task_type = NA_character_,
    backend = NULL,
    properties = character(0L),
    row_roles = NULL,
    col_roles = NULL,
    col_info = NULL,
    measures = list(),

    initialize = function(id, backend) {
      self$id = assert_id(id)
      self$backend = assert_backend(backend)

      rn = backend$rownames
      self$row_roles = list(use = rn, validation = vector(typeof(rn), 0L))
      self$col_roles = named_list(capabilities$task_col_roles, character(0L))
      self$col_roles$feature = feature = setdiff(backend$colnames, backend$primary_key)

      self$col_info = col_info(backend)
    },

    print = function(...) {
      task_print(self)
    },

    data = function(rows = NULL, cols = NULL) {
      task_data(self, rows, cols)
    },

    head = function(n = 6L) {
      assert_count(n)
      ids = head(self$row_roles$use, n)
      cols = c(self$col_roles$target, self$col_roles$feature)
      self$data(rows = ids, cols = cols)
    },

    levels = function(col) {
      assert_choice(col, self$col_info$id)
      self$col_info[list(col), "levels", with = FALSE, nomatch = 0L][[1L]][[1L]]
    },

    row_ids = function(subset = NULL) {
      if (is.null(subset)) self$row_roles$use else intersect(self$row_roles$use, subset)
    },

    filter = function(rows) {
      self$row_roles$use = intersect(self$row_roles$use, rows)
      self
    },

    select = function(cols) {
      self$col_roles$feature = setdiff(self$col_roles$feature, cols)
      self
    },

    rbind = function(data) {
      task_rbind(self, data)
    },

    cbind = function(data) {
      task_cbind(self, data)
    },

    set_row_role = function(rows, new_roles, exclusive = TRUE) {
      assert_subset(new_roles, capabilities$task_row_roles)
      assert_flag(exclusive)

      for (role in new_roles)
        self$row_roles[[role]] = union(self$row_roles[[role]], rows)

      if (exclusive) {
        for (role in setdiff(names(self$row_roles), new_roles))
          self$row_roles[[role]] = setdiff(self$row_roles[[role]], rows)
      }

      private$.hash = NA_character_
      invisible(self)
    },

    set_col_role = function(cols, new_roles, exclusive = TRUE) {
      assert_subset(new_roles, capabilities$task_col_roles)
      assert_flag(exclusive)

      for (role in new_roles)
        self$col_roles[[role]] = union(self$col_roles[[role]], cols)

      if (exclusive) {
        for (role in setdiff(names(self$col_roles), new_roles))
          self$col_roles[[role]] = setdiff(self$col_roles[[role]], cols)
      }

      private$.hash = NA_character_
      invisible(self)
    }
  ),

  active = list(
    hash = function() {
      if (is.na(private$.hash))
        private$.hash = digest::digest(list(self$id, self$row_roles, self$col_roles), algo = "xxhash64")
      private$.hash
    },

    feature_names = function() {
      self$col_roles$feature
    },

    target_names = function() {
      self$col_roles$target
    },

    nrow = function() {
      length(self$row_roles$use)
    },

    ncol = function() {
      length(self$col_roles$feature) + length(self$col_roles$target)
    },

    feature_types = function() {
      self$col_info[list(self$col_roles$feature), c("id", "type"), on = "id"]
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
      # NB: DataBackends are never copied!
      value
    }
  )
)

task_data = function(self, rows = NULL, cols = NULL) {
  order = self$col_roles$order

  if (is.null(rows)) {
    selected_rows = self$row_roles$use
  } else {
    assert_subset(rows, self$row_roles$use)
    selected_rows = rows
  }

  if (is.null(cols)) {
    selected_cols = c(self$col_roles$target, self$col_roles$feature)
  } else {
    assert_subset(cols, c(self$col_roles$target, self$col_roles$feature))
    selected_cols = cols
  }

  extra_cols = character(0L)
  if (length(order)) {
    extra_cols = setdiff(order, selected_cols)
    selected_cols = union(selected_cols, extra_cols)
  }

  data = self$backend$data(rows = selected_rows, cols = selected_cols)

  if (nrow(data) != length(selected_rows)) {
    stopf("DataBackend did not return the rows correctly: %i requested, %i received", length(selected_rows), nrow(data))
  }

  if (ncol(data) != length(selected_cols)) {
    stopf("DataBackend did not return the cols correctly: %i requested, %i received", length(selected_cols), ncol(data))
  }

  if (length(order)) {
    setorderv(data, order)
  }

  if (length(extra_cols)) {
    data[, (extra_cols) := NULL]
  }

  return(data[])
}


# Performs the following steps to virtually rbind data to the task:
# 1. Check that an rbind is feasible
# 2. Update row_roles
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
  assert_set_equal(names(data), c(self$col_roles$feature, self$col_roles$target, pk))

  ## 1.3 Check that there are now duplicated row_ids
  if (!auto_incremented) {
    tmp = self$backend$data(data[[pk]], pk)[[1L]]
    if (length(tmp))
      stopf("Cannot rbind task: Duplicated row ids: %s", stri_head(tmp))
  }

  ## 1.4 Check that types are matching
  data_col_info = col_info(data, primary_key = pk)
  joined = merge(self$col_info, data_col_info, by = "id")
  tmp = head(joined[get("type.x") != get("type.y")], 1L)
  if (nrow(tmp)) {
    stopf("Cannot rbind task: Types do not match for column: %s (%s != %s)", tmp$id, tmp$type.x, tmp$type.y)
  }

  # 2. Update row_roles
  self$row_roles$use = c(self$row_roles$use, data[[pk]])

  # 3. Update col_info
  self$col_info$levels = Map(union, joined$levels.x, joined$levels.y)

  # 4. Overwrite self$backend with new backend
  self$backend = DataBackendRbind$new(self$backend, DataBackendDataTable$new(data, primary_key = pk))
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

  ## 1.1 Check primary key column
  if (pk %nin% names(data)) {
    stopf("Cannot cbind task: Missing primary key column '%s'", self$backend$primary_key)
  }

  assert_atomic_vector(data[[pk]], any.missing = FALSE, unique = TRUE)
  if (self$col_info[list(pk), "type", on = "id"][[1L]] != class(data[[pk]])) {
    stopf("Cannot cbind task: Primary key column '%s' has wrong type", self$backend$primary_key)
  }

  ## 1.2 Check that there are no duplicated column names
  tmp = setdiff(intersect(self$col_info$id, names(data)), pk)
  if (length(tmp)) {
    stopf("Cannot cbind task: Duplicated column names: %s", stri_head(tmp))
  }

  ## 1.3 Check for set equality of row ids
  if (self$backend$data(data[[pk]], pk)[, .N] != nrow(data)) {
    stopf("Cannot cbind task: Row ids do not match")
  }

  # 2. Update col_info
  data_col_info = col_info(data)
  self$col_info = setkeyv(rbindlist(list(self$col_info, data_col_info[!list(pk)])), "id")
  self$col_roles$feature = c(self$col_roles$feature, setdiff(names(data), pk))

  # 3. Overwrite self$backend with new backend
  self$backend = DataBackendCbind$new(self$backend, DataBackendDataTable$new(data, primary_key = pk))
}

task_print = function(self) {
  catf("Task '%s' of type %s (%i x %i)", self$id, self$task_type, self$nrow, self$ncol)
  catf(stri_wrap(initial = "Target: ", self$target_names))
  catf(stri_wrap(initial = "Features: ", self$feature_names))
  if (length(self$col_roles$order))
    catf(stri_wrap(initial = "Order by: ", self$col_roles$order))
  catf(stri_wrap(initial = "\nPublic: ", setdiff(ls(self), c("initialize", "print"))))
}


col_info = function(x, ...) {
  UseMethod("col_info")

}

col_info.data.table = function(x, primary_key = character(0L), ...) {
  types = vcapply(x, class)
  discrete = setdiff(names(types)[types %in% c("character", "factor")], primary_key)
  levels = insert(named_list(names(types)), lapply(x[, discrete, with = FALSE], distinct))
  data.table(id = names(types), type = unname(types), levels = levels, key = "id")
}

col_info.DataBackend = function(x, ...) {
  types = vcapply(x$head(1L), class)
  discrete = setdiff(names(types)[types %in% c("character", "factor")], x$primary_key)
  levels = insert(named_list(names(types)), x$distinct(discrete))
  data.table(id = names(types), type = unname(types), levels = levels, key = "id")
}

#' @title Abstract learning task
#'
#' @description
#' This is the abstract base class for task objects like [TaskClassif] and [TaskRegr].
#'
#' @section Usage:
#' ```
#' # Construction
#' t = Task$new(id, backend, task_type)
#' #
#' t$id
#' t$task_type
#' t$backend
#' t$row_roles
#' t$col_info
#' t$set_row_role(rows, new_roles, exclusive = TRUE)
#' t$set_col_role(cols, new_roles, exclusive = TRUE)
#' t$measures
#' t$data(rows = NULL, cols = NULL)
#' t$head(n = 6)
#' t$levels(col)
#' t$row_ids
#' t$features_names
#' t$target_names
#' t$nrow
#' t$ncol
#' t$feature_types
#' t$formula
#' t$hash
#' #
#' t$filter(rows)
#' t$select(cols)
#' t$rbind(data)
#' t$cbind(data)
#' t$overwrite(data)
#' ```
#'
#' @section Arguments:
#' * `id` (`character(1)`):
#'   Name of the task.
#' * `backend` ([DataBackend]):
#'   [DataBackend] which stores the data.
#' * `task_type` (`character(1)`):
#'   Task type. Set via class which inherits from [Task].
#' * `data` ([data.frame()]):
#'   New data to rbind/cbind to the task.
#' * `rows` (`integer()` | `character()`):
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
#'
#' @section Details:
#' * `$new()` initializes a new object of class [Task].
#'
#' * `$id` (`character(1)`) stores the name of the task.
#'
#' * `$backend()` ([DataBackend]) stores the [DataBackend] of the task.
#'
#' * `$row_roles` (`list`). Stores the row ids of [DataBackend] in vectors of row roles:
#'   - `"use"`: Use in training.
#'   - `"validation"`: Do not use in training, this are (possibly unlabeled) observations
#'     which are held back unless explicitly addressed.
#'   To alter the role, use `set_row_role()`.
#'
#' * `$col_roles` (`list`). Each column (feature)
#'   can have a specific mutually exclusive role in the learning task:
#'   - `"feature"`: Regular feature.
#'   - `"target"`: Column with target labels.
#'   - `"ignore"`: Do not these features at all.
#'   - `"primary_key"`: Name of the primary id column used in [DataBackend].
#'   To alter the role, use `set_col_role()`.
#'
#' * `$col_info` (`data.table`) with columns `id`, and `type` and `levels`.
#'   Stores column names of [DataBackend] in column `id`.
#'   Column `type` stores the storage type of the variables, e.g. `integer`, `numeric` or `character`.
#'   Column `levels` stores the levels for factor and character variables.
#'
#' * `$set_row_role()` sets the role for specified rows, referenced by row id.
#'   If `exclusive` is `TRUE`, the referenced rows will be removed from all other roles.
#'
#' * `$set_col_role()` sets the role for specified columns, referenced by name.
#'   If `exclusive` is `TRUE`, the referenced columns will be removed from all other roles.
#'
#' * `$measures` (`list` of [Measure]) stores the default measures for this task.
#'
#' * `$data()` is used to retrieve data from the backend as `data.table`.
#'   Rows are subsetted to only contain observations with `role == "use"`.
#'   Columns are filtered to only contain features with `role %in% c("target", "feature")`.
#'   If invalid `rows` or `cols` are specified, an exception is raised.
#'
#' * `$head()` ([data.table::data.table()]) can be used to peek into the first `n` observations with `role == "use"`.
#'
#' * `$levels()` (`character()`) queries the distinct levels of the column `col`. Only works for `character` and `factor` columns.
#'   This function ignores the row roles, so you get all levels found in the [DataBackend].
#'
#' * `$row_ids` (`data.table()`] returns the active row ids used in the backend, i.e. subsetted to observations with `role == "use"`.
#'    The column names of the returned `data.table` equals the primary key column in the [DataBackend].
#'
#' * `$feature_names` (`character()`) returns all column names with `role == "feature"`.
#'
#' * `$target_names` (`character()`) returns all column names with `role == "target"`.
#'
#' * `$nrow` (`integer(1)`) provides the total number of rows with `role == "use"`.
#'
#' * `$ncol` (`integer(1)`) provides the total number of cols with `role %in% c("target", "feature")`.
#'
#' * `$feature_types` [`data.table::data.table()`) returns a table with columns `id` and `type` where `id` are the column names of "active"
#'   features of the task and `type` is the storage type.
#'
#' * `$formula` constructs a [stats::formula], e.g. `[target] ~ [feature_1] + [feature_2] + ... + [feature_k]`.
#'
#' * `$filter()` reduces the task, subsetting it to only the rows specified.
#'
#' * `$select()` reduces the task, subsetting it to only the columns specified.
#'
#' * `$rbind()` extends the task with additional rows.
#'
#' * `$cbind()` extends the task with additional columns.
#'   The row ids must be provided as column in `data` (with column name matching the primary key name of the [DataBackend]).
#'
#' * `$overwrite()` overwrite the data in the [DataBackend] with data provided as [data.table::data.table()].
#'   The row ids must be provided as column in `data` (with column name matching the primary key name of the [DataBackend]).
#'
#' * `$hash` (`character(1)`) stores a checksum calculated on the `id`, `row_roles` and `col_info`.
#'
#' @section Task mutators:
#' The methods `filter()`, `select()`, `rbind()`, `cbind()`, and `overwrite()` change the task in-place,
#' but without modifying the [DataBackend].
#' `filter()` and `select()` just reduce the set of active rows or columns, providing a different view on the data.
#' `rbind()`, `cbind()`, and `overwrite()` first create a new [DataBackendDataTable] from the provided data, and then
#' merge both backends into an abstract [DataBackend] which combines the results on-demand.
#'
#'
#' @name Task
#' @export
#' @family Task
#' @examples
#' b = as_data_backend(iris)
#' task = Task$new("iris", task_type = "classif", backend = b)
#'
#' task$nrow
#' task$ncol
#' task$head()
#' task$feature_names
#' task$formula
#'
#' # Remove "Petal.Length"
#' task$set_col_role("Petal.Length", character(0L))
#'
#' # Remove "Petal.Width", alternative way
#' task$select(setdiff(task$feature_names, "Petal.Width"))
#'
#' task$feature_names
#'
#' # Add new column "foo"
#' task$cbind(cbind(data.frame(foo = 1:150), task$row_ids))
NULL

#' @include reflections.R
Task = R6Class("Task",
  cloneable = TRUE,
  public = list(
    id = NULL,
    task_type = NULL,
    backend = NULL,
    properties = character(0L),
    row_roles = NULL,
    col_roles = NULL,
    col_info = NULL,
    measures = list(),

    initialize = function(id, task_type, backend) {
      self$id = assert_id(id)
      self$task_type = assert_choice(task_type, mlr_reflections$task_types)
      self$backend = assert_backend(backend)
      self$col_info = col_info(backend)

      rn = backend$rownames
      cn = self$col_info$id

      self$row_roles = list(use = rn, validation = rn[0L])
      self$col_roles = named_list(mlr_reflections$task_col_roles[[task_type]], character(0L))
      self$col_roles$feature = setdiff(cn, backend$primary_key)
    },

    print = function(...) {
      task_print(self)
    },

    data = function(rows = NULL, cols = NULL, format = NULL) {
      task_data(self, rows, cols, format)
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

    filter = function(rows) {
      self$row_roles$use = intersect(self$row_roles$use, rows)
      invisible(self)
    },

    select = function(cols) {
      self$col_roles$feature = intersect(self$col_roles$feature, cols)
      invisible(self)
    },

    rbind = function(data) {
      task_rbind(self, data)
    },

    cbind = function(data) {
      task_cbind(self, data)
    },

    overwrite = function(data) {
      task_overwrite(self, data)
    },

    set_row_role = function(rows, new_roles, exclusive = TRUE) {
      assert_subset(new_roles, mlr_reflections$task_row_roles)
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
      assert_subset(new_roles, mlr_reflections$task_col_roles[[self$task_type]])
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

    row_ids = function() {
      res = data.table(..row_id = self$row_roles$use)
      setnames(res, "..row_id", self$backend$primary_key)
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

task_data = function(self, rows = NULL, cols = NULL, format = NULL) {
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

  data = self$backend$data(rows = selected_rows, cols = selected_cols, format = format %??% self$backend$formats[1L])

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
  types = map_chr(x, class)
  discrete = setdiff(names(types)[types %in% c("character", "factor")], primary_key)
  levels = insert_named(named_list(names(types)), lapply(x[, discrete, with = FALSE], distinct))
  data.table(id = names(types), type = unname(types), levels = levels, key = "id")
}

col_info.DataBackend = function(x, ...) {
  types = map_chr(x$head(1L), class)
  discrete = setdiff(names(types)[types %in% c("character", "factor")], x$primary_key)
  levels = insert_named(named_list(names(types)), x$distinct(discrete))
  data.table(id = names(types), type = unname(types), levels = levels, key = "id")
}

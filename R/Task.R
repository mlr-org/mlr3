#' @title Task Class
#'
#' @usage NULL
#' @format [R6::R6Class] object.
#' @include mlr_reflections.R
#'
#' @description
#' This is the abstract base class for task objects like [TaskClassif] and [TaskRegr].
#'
#' Tasks serve two purposes:
#'
#' 1. Tasks wrap a [DataBackend], an object to transparently interface different data storage types.
#' 2. Tasks store meta-information, such as the role of the individual columns in the [DataBackend].
#'    For example, for a classification task a single column must be marked as target column, and others as features.
#'
#' Predefined (toy) tasks are stored in the [Dictionary] [mlr_tasks],
#' e.g. [`iris`][mlr_tasks_iris] or [`boston_housing`][mlr_tasks_boston_housing].
#'
#' @section Construction:
#' Note: This object is typically constructed via a derived classes, e.g. [TaskClassif] or [TaskRegr].
#'
#' ```
#' t = Task$new(id, task_type, backend)
#' ```
#'
#' * `id` :: `character(1)`\cr
#'   Identifier for the task.
#'
#' * `task_type` :: `character(1)`\cr
#'   Set in the classes which inherit from this class.
#'   Must be an element of [mlr_reflections$task_types][mlr_reflections].
#'
#' * `backend` :: [DataBackend]\cr
#'   Either a [DataBackend], or any object which is convertible to a DataBackend with `as_data_backend()`.
#'   E.g., a `data.frame()` will be converted to a [DataBackendDataTable].
#'
#' @section Fields:
#' * `backend` :: [DataBackend].
#'
#' * `col_info` :: [data.table::data.table()]\cr
#'   Table with with 3 columns:
#'   - `"id"` stores the name of the column.
#'   - `"type"` holds the storage type of the variable, e.g. `integer`, `numeric` or `character`.
#'   - `"levels"` stores a vector of distinct values (levels) for factor and character variables.
#'
#' * `col_roles` :: named `list()`\cr
#'   Each column (feature) can have an arbitrary number of the following roles:
#'     - `"feature"`: Regular feature used in the model fitting process.
#'     - `"target"`: Target variable.
#'     - `"label"`: Observation labels. May be used in plots.
#'     - `"order"`: Data returned by `$data()` is ordered by this column (or these columns).
#'     - `"groups"`: During resampling, observations with the same value of the variable with role "groups"
#'          are marked as "belonging together". They will be exclusively assigned to be either in the training set
#'          or in the test set for each resampling iteration. Only up to one column may have this role.
#'     - `"weights"`: Observation weights. Only up to one column may have this role.
#'
#'   `col_roles` keeps track of the roles with a named list of vectors of feature names.
#'   To alter the roles, use `t$set_col_role()`.
#'
#' * `row_roles` :: named `list()`\cr
#'   Each row (observation) can have an arbitrary number of roles in the learning task:
#'     - `"use"`: Use in train / predict / resampling.
#'     - `"validation"`: Hold the observations back unless explicitly requested.
#'       Validation sets are not yet completely integrated into the package.
#'
#'   `row_roles` keeps track of the roles with a named list of vectors of feature names.
#'   To alter the role, use `set_row_role()`.
#'
#' * `feature_names` :: `character()`\cr
#'   Return all column names with `role == "feature"`.
#'
#' * `feature_types` :: [data.table::data.table()]\cr
#'   Returns a table with columns `id` and `type` where `id` are the column names of "active" features of the task
#'   and `type` is the storage type.
#'
#' * `hash` :: `character(1)`\cr
#'   Hash (unique identifier) for this object.
#'
#' * `id` :: `character(1)`\cr
#'   Identifier of the Task.
#'
#' * `ncol` :: `integer(1)`\cr
#'   Returns the total number of cols with role "target" or "feature".
#'
#' * `nrow` :: `integer(1)`\cr
#'   Return the total number of rows with role "use".
#'
#' * `row_ids` :: (`integer()` | `character()`)\cr
#'   Returns the row ids of the [DataBackend] for observations with with role "use".
#'
#' * `target_names` :: `character()`\cr
#'   Returns all column names with role "target".
#'
#' * `task_type` :: `character(1)`\cr
#'   Stores the type of the [Task].
#'
#' * `properties` :: `character()`\cr
#'   Set of task properties. Possible properties are are stored in
#'   [mlr_reflections$task_properties][mlr_reflections].
#'
#' * `groups` :: [data.table::data.table()]\cr
#'   If the task has a designated column role "groups", table with two columns:
#'   `row_id` (`integer()` | `character()`) and the grouping variable `group` (`vector()`).
#'   Returns `NULL` if there are is no grouping column.
#'
#' * `weights` :: [data.table::data.table()]\cr
#'   If the task has a designated column role "weights", table with two columns:
#'   `row_id` (`integer()` | `character()`) and the observation weights `weight` (`numeric()`).
#'   Returns `NULL` if there are is no weight column.
#'
#' @section Methods:
#' * `data(rows = NULL, cols = NULL, data_format = NULL)`\cr
#'   (`integer()` | `character()`, `character(1)`, `character(1)`) -> `any`\cr
#'   Returns a slice of the data from the [DataBackend] in the data format specified by `data_format`
#'   (depending on the [DataBackend], but usually a [data.table::data.table()]).
#'
#'   Rows are additionally subsetted to only contain observations with role "use", and
#'   columns are filtered to only contain features with roles "target" and "feature".
#'   If invalid `rows` or `cols` are specified, an exception is raised.
#'
#' * `formula(rhs = NULL)`\cr
#'   `character()` -> [stats::formula()]\cr
#'   Constructs a [stats::formula()], e.g. `[target] ~ [feature_1] + [feature_2] + ... + [feature_k]`, using
#'   the features provided in argument `rhs` (defaults to all columns with role `"feature"`).
#'
#' * `levels(cols = NULL)`\cr
#'   `character()` -> named `list()`\cr
#'   Returns the distinct values for columns referenced in `cols` with storage type "character", "factor" or "ordered".
#'   Argument `cols` defaults to all such columns with role `"target"` or `"feature"`.
#'
#'   Note that this function ignores the row roles, it returns all levels available in the [DataBackend].
#'   To update the stored level information, e.g. after filtering a task, call `$droplevels()`.
#'
#' * `droplevels(cols = NULL)`\cr
#'   `character()` -> `self`\cr
#'   Updates the cache of stored factor levels, removing all levels not present in the current set of active rows.
#'   `cols` defaults to all columns with storage type "character", "factor", or "ordered".
#'
#' * `missings(cols = NULL)`\cr
#'   `character()` -> named `integer()`\cr
#'   Returns the number of missing observations for columns referenced in `cols`.
#'   Considers only active rows with row role `"use"`.
#'   Argument `cols` defaults to all columns with role "target" or "feature".
#'
#' * `head(n = 6)`\cr
#'   `integer()` -> [data.table::data.table()]\cr
#'   Get the first `n` observations with role `"use"`.
#'
#' * `set_col_role(cols, new_roles, exclusive = TRUE)`\cr
#'   (`character()`, `character()`, `logical(1)`) -> `self`\cr
#'   Adds the roles `new_roles` to columns referred to by `cols`.
#'   If `exclusive` is `TRUE`, the referenced columns will be removed from all other roles.
#'
#' * `set_row_role(rows, new_roles, exclusive = TRUE)`\cr
#'   (`character()`, `character()`, `logical(1)`) -> `self`\cr
#'   Adds the roles `new_roles` to rows referred to by `rows`.
#'   If `exclusive` is `TRUE`, the referenced rows will be removed from all other roles.
#'
#' * `filter(rows)`\cr
#'   (`integer()` | `character()`) -> `self`\cr
#'   Subsets the task, reducing it to only keep the rows specified in `rows`.
#'   This mutates the task in-place.
#'   See the section on task mutators for more information.
#'
#' * `select(cols)`\cr
#'   `character()` -> `self`\cr
#'   Subsets the task, reducing it to only keep the features specified in `cols`.
#'   Note that you cannot deselect the target column, for obvious reasons.
#'   This mutates the task in-place.
#'   See the section on task mutators for more information.
#'
#' * `cbind(data)`\cr
#'   `data.frame()` -> `self`\cr
#'   Extends the [DataBackend] with additional columns.
#'   The row ids must be provided as column in `data` (with column name matching the primary key name of the [DataBackend]).
#'   If this column is missing, it is assumed that the rows are exactly in the order of `t$row_ids`.
#'   This mutates the task in-place.
#'   See the section on task mutators for more information.
#'
#' * `rbind(data)`\cr
#'   `data.frame()` -> `self`\cr
#'   Extends the [DataBackend] with additional rows.
#'   The new row ids must be provided as column in `data`.
#'   If this column is missing, new row ids are constructed automatically.
#'   This mutates the task in-place.
#'   See the section on task mutators for more information.
#'
#'
#' @section S3 methods:
#' * `as.data.table(t)`\cr
#'   [Task] -> [data.table::data.table()]\cr
#'   Returns the complete data as [data.table::data.table()].
#'
#' @section Task mutators:
#' The following methods change the task in-place:
#' * `set_row_role()` and `set_col_role()` alter the row or column information in `row_roles` or `col_roles`, respectively.
#'   This provides a different "view" on the data without altering the data itself.
#' * `filter()` and `select()` subset the set of active rows or features in `row_roles` or `col_roles`, respectively.
#'   This provides a different "view" on the data without altering the data itself.
#' * `rbind()` and `cbind()` change the task in-place by binding rows or columns to the data, but without modifying the original [DataBackend].
#'   Instead, the methods first create a new [DataBackendDataTable] from the provided new data, and then
#'   merge both backends into an abstract [DataBackend] which combines the results on-demand.
#'
#' @family Task
#' @export
#' @examples
#' # we use the inherited class TaskClassif here,
#' # Class Task is not intended for direct use
#' task = TaskClassif$new("iris", iris, target = "Species")
#'
#' task$nrow
#' task$ncol
#' task$feature_names
#' task$formula()
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
#' task$cbind(data.frame(foo = 1:150))
#' task$head()
Task = R6Class("Task",
  public = list(
    id = NULL,
    task_type = NULL,
    backend = NULL,
    properties = character(0L),
    row_roles = NULL,
    col_roles = NULL,
    col_info = NULL,

    initialize = function(id, task_type, backend) {

      self$id = assert_string(id, min.chars = 1L)
      self$task_type = assert_choice(task_type, mlr_reflections$task_types)
      if (!inherits(backend, "DataBackend")) {
        self$backend = as_data_backend(backend)
      } else {
        self$backend = assert_backend(backend)
      }

      self$col_info = col_info(self$backend)
      assert_names(self$col_info$id, "strict", .var.name = "feature names")
      assert_subset(self$col_info$type, mlr_reflections$task_feature_types, .var.name = "feature types")
      pmap(self$col_info[, c("id", "levels")],
        function(id, levels) {
          assert_character(levels, any.missing = FALSE, min.chars = 1L, min.len = 1L, null.ok = TRUE,
            .var.name = sprintf("levels of '%s'", id))
        }
      )

      rn = self$backend$rownames
      cn = self$col_info$id

      self$row_roles = list(use = rn, validation = rn[0L])
      self$col_roles = named_list(mlr_reflections$task_col_roles[[task_type]], character(0L))
      self$col_roles$feature = setdiff(cn, self$backend$primary_key)
    },

    format = function() {
      sprintf("<%s:%s>", class(self)[1L], self$id)
    },

    print = function(...) {
      task_print(self)
    },

    data = function(rows = NULL, cols = NULL, data_format = "data.table") {
      task_data(self, rows, cols, data_format)
    },

    formula = function(rhs = NULL) {
      formulate(self$target_names, rhs %??% ".")
    },

    head = function(n = 6L) {
      assert_count(n)
      ids = head(self$row_roles$use, n)
      cols = c(self$col_roles$target, self$col_roles$feature)
      self$data(rows = ids, cols = cols)
    },

    levels = function(cols = NULL) {
      if (is.null(cols)) {
        cols = unlist(self$col_roles[c("target", "feature")], use.names = FALSE)
        cols = self$col_info[id %in% cols & type %in% c("character", "factor", "ordered"), "id", with = FALSE][[1L]]
      } else {
        assert_subset(cols, self$col_info$id)
      }

      set_names(self$col_info[list(cols), "levels", with = FALSE][[1L]], cols)
    },

    missings = function(cols = NULL) {
      if (is.null(cols)) {
        cols = unlist(self$col_roles[c("target", "feature")], use.names = FALSE)
      } else {
        assert_subset(cols, self$col_info$id)
      }

      self$backend$missings(self$row_ids, cols = cols)
    },

    filter = function(rows) {
      rows = assert_row_ids(rows, type = typeof(self$row_roles$use))
      self$row_roles$use = intersect(self$row_roles$use, rows)
      invisible(self)
    },

    select = function(cols) {
      assert_subset(cols, self$col_roles$feature)
      self$col_roles$feature = intersect(self$col_roles$feature, cols)
      invisible(self)
    },

    rbind = function(data) {
      task_rbind(self, data)
      invisible(self)
    },

    cbind = function(data) {
      task_cbind(self, data)
      invisible(self)
    },

    set_row_role = function(rows, new_roles, exclusive = TRUE) {
      task_set_row_role(self, rows, new_roles, exclusive)
      invisible(self)
    },

    set_col_role = function(cols, new_roles, exclusive = TRUE) {
      task_set_col_role(self, cols, new_roles, exclusive)
      invisible(self)
    },

    droplevels = function(cols = NULL) {
      ids = self$col_info[type %in% c("character", "factor", "ordered"), "id", with = FALSE][[1L]]
      cols = if (is.null(cols)) ids else intersect(cols, ids)
      lvls = self$backend$distinct(rows = self$row_ids, cols = cols)
      self$col_info = ujoin(self$col_info, enframe(lvls, "id", "levels"), key = "id")
      invisible(self)
    }
  ),

  active = list(
    hash = function() {
      hash(list(
        class(self), self$id, self$backend$hash, self$row_roles, self$col_roles,
        self$col_info$levels, self$properties
      ))
    },

    row_ids = function() {
      self$row_roles$use
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
      setkeyv(self$col_info[list(self$col_roles$feature), c("id", "type"), on = "id"], "id")
    },

    data_formats = function() {
      self$backend$data_formats
    },

    groups = function() {
      groups = self$col_roles$groups
      if (length(groups) == 0L) {
        return(NULL)
      }
      data = self$backend$data(self$row_roles$use, c(self$backend$primary_key, groups))
      setnames(data, names(data), c("row_id", "group"))[]
    },

    weights = function() {
      weights = self$col_roles$weights
      if (length(weights) == 0L) {
        return(NULL)
      }
      data = self$backend$data(self$row_roles$use, c(self$backend$primary_key, weights))
      setnames(data, names(data), c("row_id", "weight"))[]
    }
  ),

  private = list(
    deep_clone = function(name, value) {
      # NB: DataBackends are never copied!
      # TODO: check if we can assume col_info to be read-only
      if (name == "col_info") copy(value) else value
    }
  )
)

task_data = function(self, rows = NULL, cols = NULL, data_format = "data.table") {

  assert_choice(data_format, self$backend$data_formats)

  if (is.null(rows)) {
    selected_rows = self$row_roles$use
  } else {
    assert_subset(rows, self$row_roles$use)
    if (is.double(rows)) {
      rows = as.integer(rows)
    }
    selected_rows = rows
  }

  if (is.null(cols)) {
    selected_cols = c(self$col_roles$target, self$col_roles$feature)
  } else {
    assert_subset(cols, c(self$col_roles$target, self$col_roles$feature))
    selected_cols = cols
  }

  order = self$col_roles$order
  if (length(order)) {
    if (data_format != "data.table") {
      stopf("Ordering only supported for data_format 'data.table'")
    }
    order_cols = setdiff(order, selected_cols)
    selected_cols = union(selected_cols, order_cols)
  } else {
    order_cols = character()
  }

  data = self$backend$data(rows = selected_rows, cols = selected_cols, data_format = data_format)

  if (length(selected_cols) && nrow(data) != length(selected_rows)) {
    stopf("DataBackend did not return the rows correctly: %i requested, %i received", length(selected_rows), nrow(data))
  }

  if (length(selected_rows) && ncol(data) != length(selected_cols)) {
    stopf("DataBackend did not return the cols correctly: %i requested, %i received", length(selected_cols), ncol(data))
  }

  if (length(order)) {
    setorderv(data, order)[]
    if (length(order_cols)) {
      data[, (order_cols) := NULL][]
    }
  }

  return(data)
}

task_print = function(self) {

  catf("%s (%i x %i)", format(self), self$nrow, self$ncol)
  catf(str_indent("Target:", self$target_names))
  catf(str_indent("Properties:", self$properties))

  types = self$feature_types
  if (nrow(types)) {
    catf("Features (%i):", nrow(types))
    types = types[, list(N = .N, feats = str_collapse(get("id"), n = 100L)), by = "type"][, "type" := translate_types(get("type"))]
    setorderv(types, "N", order = -1L)
    pmap(types, function(type, N, feats) catf(str_indent(sprintf("* %s (%i):", type, N), feats)))
  }

  if (length(self$col_roles$order)) {
    catf(str_indent("Order by:", self$col_roles$order))
  }
  if ("groups" %in% self$properties) {
    catf(str_indent("Groups:", self$col_roles$groups))
  }
  if ("weights" %in% self$properties) {
    catf(str_indent("Weights:", self$col_roles$weights))
  }
}

# collect column information of a backend.
# This currently includes:
# * storage type
# * levels (for character / factor / ordered), but not for the primary key column
col_info = function(x, ...) {
  UseMethod("col_info")
}

col_info.data.table = function(x, primary_key = character(0L), ...) {
  types = map_chr(x, function(x) class(x)[1L])
  discrete = setdiff(names(types)[types %in% c("character", "factor", "ordered")], primary_key)
  levels = insert_named(named_list(names(types)), lapply(x[, discrete, with = FALSE], distinct, drop = FALSE))
  data.table(id = names(types), type = unname(types), levels = levels, key = "id")
}

col_info.DataBackend = function(x, ...) {
  types = map_chr(x$head(1L), function(x) class(x)[1L])
  discrete = setdiff(names(types)[types %in% c("character", "factor", "ordered")], x$primary_key)
  levels = insert_named(named_list(names(types)), x$distinct(rows = NULL, cols = discrete))
  data.table(id = names(types), type = unname(types), levels = levels, key = "id")
}

#' @export
as.data.table.Task = function(x, ...) {
  x$head(x$nrow)
}

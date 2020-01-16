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
#' Predefined (toy) tasks are stored in the [mlr3misc::Dictionary] [mlr_tasks],
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
#'   Must be an element of [mlr_reflections$task_types$type][mlr_reflections].
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
#'   - `"levels"` stores a vector of distinct values (levels) for factor variables.
#'
#' * `col_roles` :: named `list()`\cr
#'   Each column (feature) can have an arbitrary number of the following roles:
#'     - `"feature"`: Regular feature used in the model fitting process.
#'     - `"target"`: Target variable.
#'     - `"name"`: Row names / observation labels. To be used in plots.
#'     - `"order"`: Data returned by `$data()` is ordered by this column (or these columns).
#'     - `"group"`: During resampling, observations with the same value of the variable with role "group"
#'          are marked as "belonging together". They will be exclusively assigned to be either in the training set
#'          or in the test set for each resampling iteration. Only up to one column may have this role.
#'     - `"stratum"`: Stratification variables. Multiple discrete columns may have this role.
#'     - `"weight"`: Observation weights. Only up to one column (assumed to be discrete) may have this role.
#'
#'   `col_roles` keeps track of the roles with a named list, the elements are named by column role and each element is a character vector of column names.
#'   To alter the roles, just modify the list, e.g. with  \R's set functions ([intersect()], [setdiff()], [union()], \ldots).
#'
#' * `row_roles` :: named `list()`\cr
#'   Each row (observation) can have an arbitrary number of roles in the learning task:
#'     - `"use"`: Use in train / predict / resampling.
#'     - `"validation"`: Hold the observations back unless explicitly requested.
#'       Validation sets are not yet completely integrated into the package.
#'
#'   `row_roles` keeps track of the roles with a named list, elements are named by row role and each element is a `integer()` or `character()` vector of row ids.
#'   To alter the roles, just modify the list, e.g. with  \R's set functions ([intersect()], [setdiff()], [union()], \ldots).
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
#'   The following properties are currently standardized and understood by tasks in \CRANpkg{mlr3}:
#'   * `"strata"`: The task is resampled using one or more stratification variables (role `"stratum"`).
#'   * `"groups"`: The task comes with grouping/blocking information (role `"group"`).
#'   * `"weights"`: The task comes with observation weights (role `"weight"`).
#'   Note that above listed properties are calculated from the `$col_roles` and must not be set explicitly.
#'
#' * `strata` :: [data.table::data.table()]\cr
#'   If the task has columns designated with role `"stratum"`, returns a table with one subpopulation per row and two columns:
#'   `N` (`integer()`) with the number of observations in the subpopulation and `row_id` (list of `integer()` | list of `character()`) as list
#'   column with the row ids in the respective subpopulation.
#'   Returns `NULL` if there are is no stratification variable.
#'
#'   See [Resampling] for more information on stratification.
#'
#' * `groups` :: [data.table::data.table()]\cr
#'   If the task has a column with designated role "group", table with two columns:
#'   `row_id` (`integer()` | `character()`) and the grouping variable `group` (`vector()`).
#'   Returns `NULL` if there are is no grouping column.
#'
#'   See [Resampling] for more information on grouping.
#'
#' * `weights` :: [data.table::data.table()]\cr
#'   If the task has a column with designated role "weight", table with two columns:
#'   `row_id` (`integer()` | `character()`) and the observation weights `weight` (`numeric()`).
#'   Returns `NULL` if there are is no weight column.
#'
#' * `man` :: `character(1)`\cr
#'   String in the format `[pkg]::[topic]` pointing to a manual page for this object.
#'
#' @section Methods:
#' * `data(rows = NULL, cols = NULL, data_format = NULL)`\cr
#'   (`integer()` | `character()`, `character()`, `character(1)`) -> `any`\cr
#'   Returns a slice of the data from the [DataBackend] in the data format specified by `data_format`
#'   (depending on the [DataBackend], but usually a [data.table::data.table()]).
#'
#'   Rows are additionally subsetted to only contain observations with role "use", and
#'   columns are filtered to only contain features with roles "target" and "feature".
#'   If invalid `rows` or `cols` are specified, an exception is raised.
#'
#' * `formula(rhs = ".")`\cr
#'   `character()` -> [stats::formula()]\cr
#'   Constructs a [stats::formula()], e.g. `[target] ~ [feature_1] + [feature_2] + ... + [feature_k]`, using
#'   the features provided in argument `rhs` (defaults to all columns with role `"feature"`, symbolized by `"."`).
#'
#' * `levels(cols = NULL)`\cr
#'   `character()` -> named `list()`\cr
#'   Returns the distinct values for columns referenced in `cols` with storage type "factor" or "ordered".
#'   Argument `cols` defaults to all such columns with role `"target"` or `"feature"`.
#'
#'   Note that this function ignores the row roles, it returns all levels available in the [DataBackend].
#'   To update the stored level information, e.g. after filtering a task, call `$droplevels()`.
#'
#' * `droplevels(cols = NULL)`\cr
#'   `character()` -> `self`\cr
#'   Updates the cache of stored factor levels, removing all levels not present in the current set of active rows.
#'   `cols` defaults to all columns with storage type "factor" or "ordered".
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
#' * `set_row_role(rows, new_roles, exclusive = TRUE)`\cr
#'   (`character()`, `character()`, `logical(1)`) -> `self`\cr
#'   Adds the roles `new_roles` to rows referred to by `rows`.
#'   If `exclusive` is `TRUE`, the referenced rows will be removed from all other roles.
#'
#'   This function is deprecated and will be removed in the next version in favor of directly modifying `$row_roles`.
#'
#' * `set_col_role(cols, new_roles, exclusive = TRUE)`\cr
#'
#'   (`character()`, `character()`, `logical(1)`) -> `self`\cr
#'   Adds the roles `new_roles` to columns referred to by `cols`.
#'   If `exclusive` is `TRUE`, the referenced columns will be removed from all other roles.
#'
#'   This function is deprecated and will be removed in the next version in favor of directly modifying `$col_roles`.
#'
#' * `filter(rows)`\cr
#'   (`integer()` | `character()`) -> `self`\cr
#'   Subsets the task, reducing it to only keep the rows specified in `rows`.
#'
#'   This operation mutates the task in-place.
#'   See the section on task mutators for more information.
#'
#' * `select(cols)`\cr
#'   `character()` -> `self`\cr
#'   Subsets the task, reducing it to only keep the features specified in `cols`.
#'   Note that you cannot deselect the target column, for obvious reasons.
#'
#'   This operation mutates the task in-place.
#'   See the section on task mutators for more information.
#'
#' * `cbind(data)`\cr
#'   `data.frame()` -> `self`\cr
#'   Adds additional columns to the [DataBackend].
#'   The row ids must be provided as column in `data` (with column name matching the primary key name of the [DataBackend]).
#'   If this column is missing, it is assumed that the rows are exactly in the order of `t$ow_ids`.
#'   In case of name clashes of column names in `data` and [DataBackend], columns in `data` have higher precedence
#'   and virtually overwrite the columns in the [DataBackend].
#'
#'   This operation mutates the task in-place.
#'   See the section on task mutators for more information.
#'
#' * `rbind(data)`\cr
#'   `data.frame()` -> `self`\cr
#'   Adds additional rows to the [DataBackend].
#'   The new row ids must be provided as column in `data`.
#'   If this column is missing, new row ids are constructed automatically.
#'   In case of name clashes of row ids, rows in `data` have higher precedence
#'   and virtually overwrite the rows in the [DataBackend].
#'
#'   This operation mutates the task in-place.
#'   See the section on task mutators for more information.
#'
#' * `rename(from, to)`\cr
#'   (`character()`, `character()`) -> `self`\cr
#'   Renames columns by mapping column names in `old` to new column names in `new`.
#'
#'   This operation mutates the task in-place.
#'   See the section on task mutators for more information.
#'
#' * `help()`\cr
#'   () -> `NULL`\cr
#'   Opens the corresponding help page referenced by `$man`.
#'
#' @section S3 methods:
#' * `as.data.table(t)`\cr
#'   [Task] -> [data.table::data.table()]\cr
#'   Returns the complete data as [data.table::data.table()].
#'
#' @section Task mutators:
#' The following methods change the task in-place:
#' * Any modification to `$col_roles` and `row_roles`.
#'   This provides a different "view" on the data without altering the data itself.
#' * `filter()` and `select()` subset the set of active rows or features in `row_roles` or `col_roles`, respectively.
#'   This provides a different "view" on the data without altering the data itself.
#' * `rbind()` and `cbind()` change the task in-place by binding rows or columns to the data, but without modifying the original [DataBackend].
#'   Instead, the methods first create a new [DataBackendDataTable] from the provided new data, and then
#'   merge both backends into an abstract [DataBackend] which combines the results on-demand.
#' * `rename()` wraps the [DataBackend] of the Task in an additional [DataBackend] which deals with the renaming. Also updates `col_roles` and `col_info`.
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
#' # de-select "Petal.Width"
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
    col_info = NULL,
    man = NA_character_,

    initialize = function(id, task_type, backend) {

      self$id = assert_string(id, min.chars = 1L)
      self$task_type = assert_choice(task_type, mlr_reflections$task_types$type)
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
          assert_character(levels, any.missing = FALSE, min.len = 1L, null.ok = TRUE,
            .var.name = sprintf("levels of '%s'", id))
        }
      )

      rn = self$backend$rownames
      private$.row_roles = list(use = rn, validation = rn[0L])
      private$.col_roles = named_list(mlr_reflections$task_col_roles[[task_type]], character())
      private$.col_roles$feature = setdiff(self$col_info$id, self$backend$primary_key)
    },

    help = function() {
      open_help(self$man)
    },

    format = function() {
      sprintf("<%s:%s>", class(self)[1L], self$id)
    },

    print = function(...) {
      task_print(self)
    },

    data = function(rows = NULL, cols = NULL, data_format = "data.table") {
      task_data(self, private, rows, cols, data_format)
    },

    formula = function(rhs = ".") {
      formulate(self$target_names, rhs)
    },

    head = function(n = 6L) {
      assert_count(n)
      ids = head(private$.row_roles$use, n)
      cols = c(private$.col_roles$target, private$.col_roles$feature)
      self$data(rows = ids, cols = cols)
    },

    levels = function(cols = NULL) {
      if (is.null(cols)) {
        cols = unlist(private$.col_roles[c("target", "feature")], use.names = FALSE)
        cols = self$col_info[id %in% cols & type %in% c("factor", "ordered"), "id", with = FALSE][[1L]]
      } else {
        assert_subset(cols, self$col_info$id)
      }

      set_names(self$col_info[list(cols), "levels", on = "id", with = FALSE][[1L]], cols)
    },

    missings = function(cols = NULL) {
      if (is.null(cols)) {
        cols = unlist(private$.col_roles[c("target", "feature")], use.names = FALSE)
      } else {
        assert_subset(cols, self$col_info$id)
      }

      self$backend$missings(self$row_ids, cols = cols)
    },

    filter = function(rows) {
      assert_row_ids(rows)
      private$.row_roles$use = intersect(private$.row_roles$use, rows)
      invisible(self)
    },

    select = function(cols) {
      assert_subset(cols, private$.col_roles$feature)
      private$.col_roles$feature = intersect(private$.col_roles$feature, cols)
      invisible(self)
    },

    rbind = function(data) {
      task_rbind(self, data)
    },

    cbind = function(data) {
      task_cbind(self, data)
    },

    rename = function(old, new) {
      task_rename(self, old, new)
    },

    set_row_role = function(rows, new_roles, exclusive = TRUE) {
      task_set_row_role(self, private, rows, new_roles, exclusive)
      invisible(self)
    },

    set_col_role = function(cols, new_roles, exclusive = TRUE) {
      task_set_col_role(self, private, cols, new_roles, exclusive)
      invisible(self)
    },

    droplevels = function(cols = NULL) {
      ids = self$col_info[type %in% c("factor", "ordered"), "id", with = FALSE][[1L]]
      cols = if (is.null(cols)) ids else intersect(cols, ids)
      lvls = self$backend$distinct(rows = self$row_ids, cols = cols)
      self$col_info = ujoin(self$col_info, enframe(lvls, "id", "levels"), key = "id")
      invisible(self)
    }
  ),

  active = list(
    hash = function() {
      hash(
        class(self), self$id, self$backend$hash, private$.row_roles, private$.col_roles,
        self$col_info$type, self$col_info$levels, self$properties
      )
    },

    row_ids = function() {
      private$.row_roles$use
    },

    feature_names = function() {
      private$.col_roles$feature
    },

    target_names = function() {
      private$.col_roles$target
    },

    properties = function(rhs) {
      if (missing(rhs)) {
        col_roles = private$.col_roles
        c(character(),
          private$.properties,
          if (length(col_roles$group)) "groups" else NULL,
          if (length(col_roles$stratum)) "strata" else NULL,
          if (length(col_roles$weight)) "weights" else NULL
        )
      } else {
        private$.properties = assert_set(rhs, .var.name = "properties")
      }
    },

    row_roles = function(rhs) {
      if (missing(rhs)) {
        return(private$.row_roles)
      }

      assert_list(rhs, .var.name = "row_roles")
      assert_names(names(rhs), "unique", permutation.of = mlr_reflections$task_row_roles, .var.name = "names of row_roles")
      rhs = map(rhs, assert_row_ids, .var.name = "elements of row_roles")

      private$.row_roles = rhs
    },

    col_roles = function(rhs) {
      if (missing(rhs)) {
        return(private$.col_roles)
      }

      qassertr(rhs, "S[1,]", .var.name = "col_roles")
      assert_names(names(rhs), "unique", permutation.of = mlr_reflections$task_col_roles[[self$task_type]], .var.name = "names of col_roles")
      assert_subset(unlist(rhs, use.names = FALSE), setdiff(self$col_info$id, self$backend$primary_key), .var.name = "elements of col_roles")

      task_set_col_roles(self, private, rhs)
    },

    nrow = function() {
      length(private$.row_roles$use)
    },

    ncol = function() {
      length(private$.col_roles$feature) + length(private$.col_roles$target)
    },

    feature_types = function() {
      setkeyv(self$col_info[list(private$.col_roles$feature), c("id", "type"), on = "id"], "id")
    },

    data_formats = function() {
      self$backend$data_formats
    },

    strata = function() {
      cols = private$.col_roles$stratum
      if (length(cols) == 0L) {
        return(NULL)
      }

      row_ids = self$row_ids
      tab = self$data(rows = row_ids, cols = cols)
      tab$..row_id = row_ids
      tab = tab[, list(..N = .N, ..row_id = list(.SD$..row_id)), by = cols, .SDcols = "..row_id"][, (cols) := NULL]
      setnames(tab, c("..N", "..row_id"), c("N", "row_id"))[]
    },

    groups = function() {
      groups = private$.col_roles$group
      if (length(groups) == 0L) {
        return(NULL)
      }
      data = self$backend$data(private$.row_roles$use, c(self$backend$primary_key, groups))
      setnames(data, c("row_id", "group"))[]
    },

    weights = function() {
      weights = private$.col_roles$weight
      if (length(weights) == 0L) {
        return(NULL)
      }
      data = self$backend$data(private$.row_roles$use, c(self$backend$primary_key, weights))
      setnames(data, c("row_id", "weight"))[]
    }
  ),

  private = list(
    .properties = NULL,
    .col_roles = NULL,
    .row_roles = NULL,

    deep_clone = function(name, value) {
      # NB: DataBackends are never copied!
      # TODO: check if we can assume col_info to be read-only
      if (name == "col_info") copy(value) else value
    }
  )
)

task_data = function(self, private, rows = NULL, cols = NULL, data_format = "data.table", subset_active = c("rows", "cols")) {

  assert_choice(data_format, self$backend$data_formats)
  row_roles = private$.row_roles
  col_roles = private$.col_roles

  if (is.null(rows)) {
    selected_rows = row_roles$use
  } else {
    if ("rows" %in% subset_active) {
      assert_subset(rows, row_roles$use)
    }
    if (is.double(rows)) {
      rows = as.integer(rows)
    }
    selected_rows = rows
  }

  if (is.null(cols)) {
    selected_cols = c(col_roles$target, col_roles$feature)
  } else {
    if ("cols" %in% subset_active) {
      assert_subset(cols, c(col_roles$target, col_roles$feature))
    }
    selected_cols = cols
  }

  order = col_roles$order
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
  catf(str_indent("* Target:", self$target_names))
  catf(str_indent("* Properties:", self$properties))

  types = self$feature_types
  if (nrow(types)) {
    id = type = NULL
    catf("* Features (%i):", nrow(types))
    types = types[, list(N = .N, feats = str_collapse(id, n = 100L)), by = "type"][, "type" := translate_types(type)]
    setorderv(types, "N", order = -1L)
    pmap(types, function(type, N, feats) catf(str_indent(sprintf("  - %s (%i):", type, N), feats, exdent = 4L)))
  }

  roles = self$col_roles
  if (length(roles$order)) {
    catf(str_indent("* Order by:", roles$order))
  }
  if ("strata" %in% self$properties) {
    catf(str_indent("* Strata:", roles$stratum))
  }
  if ("groups" %in% self$properties) {
    catf(str_indent("* Groups:", roles$group))
  }
  if ("weights" %in% self$properties) {
    catf(str_indent("* Weights:", roles$weight))
  }
}

# collect column information of a backend.
# This currently includes:
# * storage type
# * levels (for character / factor / ordered), but not for the primary key column
col_info = function(x, ...) {
  UseMethod("col_info")
}

col_info.data.table = function(x, primary_key = character(), ...) {
  types = map_chr(x, function(x) class(x)[1L])
  discrete = setdiff(names(types)[types %in% c("factor", "ordered")], primary_key)
  levels = insert_named(named_list(names(types)), lapply(x[, discrete, with = FALSE], distinct_values, drop = FALSE))
  data.table(id = names(types), type = unname(types), levels = levels, key = "id")
}

col_info.DataBackend = function(x, ...) {
  types = map_chr(x$head(1L), function(x) class(x)[1L])
  discrete = setdiff(names(types)[types %in% c("factor", "ordered")], x$primary_key)
  levels = insert_named(named_list(names(types)), x$distinct(rows = NULL, cols = discrete))
  data.table(id = names(types), type = unname(types), levels = levels, key = "id")
}

#' @export
as.data.table.Task = function(x, ...) {
  x$head(x$nrow)
}

task_rm_data = function(task) {
  no_row = task$row_roles$use[0L]
  task$backend = as_data_backend(task$head(0L))
  task$row_roles = list(use = no_row, validation = no_row)
  task
}

#' @title Task Class
#'
#' @usage NULL
#' @format [R6::R6Class] object.
#'
#' @description
#' This is the abstract base class for task objects like [TaskClassif] and [TaskRegr].
#'
#' @section Construction:
#' ```
#' t = Task$new(id, task_type, backend)
#' ```
#' * `id` :: `character(1)`\cr
#'   Name of the task.
#'
#' * `task_type` :: `character(1)`\cr
#'   Set in the classes which inherit from this class.
#'   Must be an element of [mlr_reflections$task_types][mlr_reflections].
#'
#' * `backend` :: [DataBackend]
#'
#'
#' @section Public:
#' * `backend` :: [DataBackend].
#'
#' * `col_info` :: [data.table::data.table()]\cr
#'   Table with with 3 columns:
#'   Column names of [DataBackend] are stored in column`id`.
#'   Column `type` holds the storage type of the variables, e.g. `integer`, `numeric` or `character`.
#'   Column `levels` keeps a list of possible levels for factor and character variables.
#'
#' * `col_roles` :: named `list()`\cr
#'   Each column (feature) can have an arbitrary number of roles in the learning task:
#'     - `"target"`: Labels to predict.
#'     - `"feature"`: Regular feature.
#'     - `"order"`: Data returned by `data()` is ordered by this column (or these columns).
#'     - `"group"`: During resampling, observations with the same value of the variable with role "group"
#'          are marked as "belonging together". They will be exclusively assigned to be either in the training set
#'          or the test set for each resampling iteration.
#'     - `"weights"`: Observation weights.
#'   `col_roles` keeps track of the roles with a named list of vectors of feature names.
#'   To alter the roles, use `t$set_col_role()`.
#'
#' * `row_roles` :: named `list()`\cr
#'   Each row (observation) can have an arbitrary number of roles in the learning task:
#'     - `"use"`: Use in train / predict / resampling.
#'     - `"validation"`: Hold the observations back unless explicitly requested.
#'   `row_roles` keeps track of the roles with a named list of vectors of feature names.
#'   To alter the role, use `set_row_role()`.
#'
#' * `feature_names` :: `character()`\cr
#'   Returns all column names with `role == "feature"`.
#'
#' * `feature_types` :: [data.table::data.table()]\cr
#'   Returns a table with columns `id` and `type` where `id` are the column names of "active" features of the task
#'   and `type` is the storage type.
#'
#'
#' * `formula` :: `formula()`\cr
#'   Constructs a [stats::formula], e.g. `[target] ~ [feature_1] + [feature_2] + ... + [feature_k]`, using
#'   the active features of the task.
#'
#' * `group` :: [data.table::data.table()]\cr
#'   Returns a table with columns `row_id` and `group` where `row_id` are the row ids and group is the value of the
#'   grouping variable. Returns `NULL` if there is no grouping.
#'
#' * `hash` :: `character(1)`\cr
#'   Hash (unique identifier) of the task.
#'
#' * `id` :: `character(1)`\cr
#'   Stores the identifier of the Task.
#'
#' * `measures` :: `list()` of [Measure]\cr
#'   Stores the measures to use for this task.
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
#'
#' @section Methods:
#' * `data(rows = NULL, cols = NULL, format = NULL)`\cr
#'   (`integer()` | `character()`, `character()`, `character(1)`) -> `any`\cr
#'   Returns a slice of the data from the [DataBackend] in the format specified by `format`
#'   (depending on the [DataBackend], but usually a [data.table::data.table()]).
#'   Rows are subsetted to only contain observations with role "use".
#'   Columns are filtered to only contain features with roles "target" and "feature".
#'   If invalid `rows` or `cols` are specified, an exception is raised.
#'
#' * `cbind(data)`\cr
#'   `data.frame()` -> `self`\cr
#'   Extends the [DataBackend] with additional columns.
#'   The row ids must be provided as column in `data` (with column name matching the primary key name of the [DataBackend]). If this column is missing, it is assumed that the rows are exactly in the order of
#'   `t$row_ids`.
#'
#' * `rbind(data)`\cr
#'   `data.frame()` -> `self`\cr
#'   Extends the [DataBackend] with additional rows.
#'   The new row ids must be provided as column in `data`.
#'   If this column is missing, new row ids are constructed automatically.
#'
#' * `filter(rows)`\cr
#'   (`integer()` | `character()`) -> `self`\cr
#'  Subsets the task, reducing it to only keep the rows specified.
#'
#' * `select(cols)`\cr
#'   `character()` -> ``self`\cr
#'   Subsets the task, reducing it to only keep the columns specified.
#'
#' * `levels(col)`\cr
#'   `character()` -> named `list()`\cr
#'   Returns  the distinct levels of the column `col`.
#'   Only applicable for features with type "character",  "factor" or "ordered".
#'   This function ignores the row roles, it returns all levels available in the [DataBackend].
#'
#' * `head(n = 6)`\cr
#'   `integer()` -> [data.table::data.table()]\cr
#'   Get the first `n` observations with role "use".
#'
#' * `replace_features(data)`\cr
#'   `data.frame()` -> `self`\cr
#'   Replaces some features of the task by constructing a completely new [DataBackendDataTable].
#'   This operation is similar to calling `select()` and `cbind()`, but explicitly copies the data.
#'
#' * `set_col_role(cols, new_roles, exclusive = TRUE)`
#'   (`character()`, `character()`, `logical(1)`) -> `self`.
#'   Adds the roles `new_roles` to columns referred to by `cols`.
#'   If `exclusive` is `TRUE`, the referenced columns will be removed from all other roles.
#'
#' * `set_row_role(rows, new_roles, exclusive = TRUE)`
#'   (`character()`, `character()`, `logical(1)`) -> `self`.
#'   Adds the roles `new_roles` to rows referred to by `rows`.
#'   If `exclusive` is `TRUE`, the referenced rows will be removed from all other roles.
#'
#' @section Task mutators:
#' The methods `filter()`, `select()`, `rbind()`, and `cbind()` change the task in-place,
#' but without modifying the [DataBackend].
#' `filter()` and `select()` just reduce the set of active rows or columns, providing a different view on the data.
#' `rbind()` and `cbind()` first create a new [DataBackendDataTable] from the provided new data, and then
#' merge both backends into an abstract [DataBackend] which combines the results on-demand.
#'
#' @family Task
#' @export
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
#' task$cbind(data.frame(foo = 1:150))
Task = R6Class("Task",
  cloneable = TRUE,
  public = list(
    task_type = NULL,
    backend = NULL,
    properties = character(0L),
    row_roles = NULL,
    col_roles = NULL,
    col_info = NULL,

    initialize = function(id, task_type, backend) {
      private$.id = assert_id(id)
      self$task_type = assert_choice(task_type, mlr_reflections$task_types)
      self$backend = assert_backend(backend)

      self$col_info = col_info(backend)
      assert_names(self$col_info$id, "strict", .var.name = "feature names")

      rn = backend$rownames
      cn = self$col_info$id

      self$row_roles = list(use = rn, validation = rn[0L])
      self$col_roles = named_list(mlr_reflections$task_col_roles[[task_type]], character(0L))
      self$col_roles$feature = setdiff(cn, backend$primary_key)
    },

    format = function() {
      sprintf("<%s:%s>", class(self)[1L], self$id)
    },

    print = function(...) {
      task_print(self)
    },

    data = function(rows = NULL, cols = NULL, format = NULL) {
      task_data(self, rows, cols, format %??% self$backend$formats[1L])
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
      private$.hash = NA_character_
      invisible(self)
    },

    select = function(cols) {
      self$col_roles$feature = intersect(self$col_roles$feature, cols)
      private$.hash = NA_character_
      invisible(self)
    },

    rbind = function(data) {
      task_rbind(self, data)
      private$.hash = NA_character_
      invisible(self)
    },

    cbind = function(data) {
      task_cbind(self, data)
      private$.hash = NA_character_
      invisible(self)
    },

    replace_features = function(data) {
      task_replace_features(self, data)
      private$.hash = NA_character_
      invisible(self)
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
    measures = function(rhs) {
      if (missing(rhs))
        return(private$.measures)
      private$.hash = NA_character_
      private$.measures = assert_measures(rhs, task = self)
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
      self$col_info[list(self$col_roles$feature), c("id", "type"), on = "id"]
    },

    formula = function() {
      generate_formula(self$target_names, self$feature_names)
    },

     group = function() {
       group = self$col_roles$group
       if (length(group) == 0L)
         return(NULL)
       data = self$backend$data(self$row_roles$use, c(self$backend$primary_key, group))
       setnames(data, names(data), c("row_id", "group"))[]
     },

     weights = function() {
       weights = self$col_roles$weights
       if (length(weights) == 0L)
         return(NULL)
       setnames(self$backend$data(self$row_roles$use, c(self$backend$primary_key, weights)), weights, "weights")
     }
  ),

  private = list(
    .measures = list(),

    .calculate_hash = function() {
      hash(list(class(self), private$.id, self$backend$hash, self$row_roles,
          self$col_roles, sort(hashes(self$measures))))
    },

    deep_clone = function(name, value) {
      # NB: DataBackends are never copied!
      # TODO: check if we can assume col_info to be read-only
      if (name == "col_info") copy(value) else value
    }
  )
)

Task = add_id_hash(Task)

task_data = function(self, rows = NULL, cols = NULL, format) {
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

  if (length(selected_cols) && nrow(data) != length(selected_rows)) {
    stopf("DataBackend did not return the rows correctly: %i requested, %i received", length(selected_rows), nrow(data))
  }

  if (length(selected_rows) && ncol(data) != length(selected_cols)) {
    stopf("DataBackend did not return the cols correctly: %i requested, %i received", length(selected_cols), ncol(data))
  }

  if (format == "data.table") {
    if (length(order)) {
      setorderv(data, order)[]
    }

    if (length(extra_cols)) {
      data[, (extra_cols) := NULL][]
    }
  }

  return(data)
}

task_print = function(self) {
  catf("%s (%i x %i)", format(self), self$nrow, self$ncol)
  catf(str_indent("Target:", str_collapse(self$target_names)))

  types = self$feature_types
  if (nrow(types)) {
    catf("Features (%i):", nrow(types))
    types = types[, list(N = .N, feats = str_collapse(get("id"), n = 100L)), by = "type"][, "type" := translate_types(get("type"))]
    setorderv(types, "N", order = -1L)
    pmap(types, function(type, N, feats) catf(str_indent(sprintf("* %s (%i):", type, N), feats)))
  }

  if (length(self$col_roles$order))
    catf(str_indent("Order by:", self$col_roles$order))
  if (length(self$col_roles$weights))
    catf(str_indent("Weights:", self$col_roles$weights))

  catf(str_indent("\nPublic:", str_r6_interface(self)))
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
  levels = insert_named(named_list(names(types)), lapply(x[, discrete, with = FALSE], distinct))
  data.table(id = names(types), type = unname(types), levels = levels, key = "id")
}

col_info.DataBackend = function(x, ...) {
  # X <<- x
  types = map_chr(x$head(1L), function(x) class(x)[1L])
  discrete = setdiff(names(types)[types %in% c("character", "factor", "ordered")], x$primary_key)
  levels = insert_named(named_list(names(types)), x$distinct(discrete))
  data.table(id = names(types), type = unname(types), levels = levels, key = "id")
}

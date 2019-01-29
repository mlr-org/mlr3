#' @title Supervised or Unsupervised Tasks
#'
#' @name Task
#' @format [R6Class] object.
#' @description
#' This is the abstract base class for task objects like [TaskClassif] and [TaskRegr].
#'
#' @section Usage:
#' ```
#' # Construction
#' t = Task$new(id, backend, task_type)
#'
#' # Members
#' t$backend
#' t$col_info
#' t$col_roles
#' t$features_names
#' t$feature_types
#' t$formula
#' t$groups
#' t$hash
#' t$id
#' t$measures
#' t$ncol
#' t$nrow
#' t$properties
#' t$row_ids
#' t$row_roles
#' t$target_names
#' t$task_type
#'
#' # Methods: Accessors
#' t$data(rows = NULL, cols = NULL)
#' t$head(n = 6)
#' t$levels(col)
#'
#' # Methods: Mutators
#' t$cbind(data)
#' t$filter(rows)
#' t$rbind(data)
#' t$select(cols)
#' t$replace_features(data, ...)
#' t$set_col_role(cols, new_roles, exclusive = TRUE)
#' t$set_row_role(rows, new_roles, exclusive = TRUE)
#' ```
#'
#' @section Arguments:
#' * `id` (`character(1)`):
#'   Name of the task.
#' * `backend` ([DataBackend]):
#'   [DataBackend] which stores the data.
#' * `task_type` (`character(1)`):
#'   Task type. Set via class which inherits from [Task].
#' * `rows` (`integer()` | `character()`):
#'   Vector of row ids specifying rows from the [DataBackend] using its primary key.
#'   Can be `character()` or `integer`, depending on the [DataBackend].
#' * `cols` (`character()`):
#'   Character vector to specify columns from the [DataBackend].
#' * `n` (`integer(1)`):
#'   Number of rows to retrieve from the [DataBackend].
#' * `col` (`character(1)`):
#'   Character vector to specify a single column from the [DataBackend].
#' * `new_roles` (`character(1)`):
#'   New roles to assign for specified rows/columns.
#' * `exclusive` (`logical(1)`):
#'   If `TRUE`, the cols/rows will be removed from all roles except `new_roles`.
#' * `data` ([data.frame()]):
#'   New data to rbind/cbind to the task.
#'
#' @section Details:
#' * `$backend` ([DataBackend]) stores the [DataBackend] of the task.
#' * `$cbind` extends the task with additional columns.
#'   The row ids must be provided as column in `data` (with column name matching the primary key name of the [DataBackend]).
#' * `$col_info` ([data.table()]) with columns `id`, `type` and `levels`.
#'   Stores column names of [DataBackend] in column `id`.
#'   Column `type` stores the storage type of the variables, e.g. `integer`, `numeric` or `character`.
#'   Column `levels` stores the levels for factor and character variables.
#' * `$col_roles` (`list`). Each column (feature)
#'   can have a specific mutually exclusive role in the learning task:
#'   - `"feature"`: Regular feature.
#'   - `"target"`: Column with target labels.
#'   - `"order"`: Returned data is ordered by these column(s).
#'   - `"groups"`: During resampling, observations with the same value of the variable with role "groups"
#'        are marked as "belonging together". They will be exclusively assigned to be either in the training set
#'        or the test set.
#'        Returns a ([data.table()]) with two columns: first column are rows ids, second column are the group labels.
#'   - `"weights"`: Observation weights. ([data.table()]) with two columns: first column are the row ids,
#'       second column are the observation weights.
#'   To alter the role, use `$set_col_role()`
#' * `$data` is used to retrieve data from the backend as [data.table()].
#'   Rows are subsetted to only contain observations with `role == "use"`.
#'   Columns are filtered to only contain features with `role %in% c("target", "feature")`.
#'   If invalid `rows` or `cols` are specified, an exception is raised.
#' * `$feature_names` (`character()`) returns all column names with `role == "feature"`.
#' * `$feature_types` ([data.table()]) returns a table with columns `id` and `type` where `id` are the column names of "active"
#'   features of the task and `type` is the storage type.
#' * `$filter` reduces the task, subsetting it to only the rows specified.
#' * `$formula` constructs a [stats::formula], e.g. `[target] ~ [feature_1] + [feature_2] + ... + [feature_k]`.
#' * `$groups` returns a ([data.table()]) with two columns: the row ids and the grouping / blocking information.
#' * `$hash` (`character(1)`) stores a checksum calculated on the `id`, `row_roles` and `col_roles`.
#' * `$head` ([data.table()]) can be used to peek into the first `n` observations with `role == "use"`.
#' * `$id` (`character(1)`) stores the name of the task.
#' * `$measures` (`list` of [Measure]) stores the default measures for this task.
#' * `$ncol` (`integer(1)`) provides the total number of cols with `role %in% c("target", "feature")`.
#' * `$nrow` (`integer(1)`) provides the total number of rows with `role == "use"`.
#' * `$row_ids` (`vector()`) returns the active row ids used in the backend, i.e. subsetted to observations with `role == "use"`.
#' * `$row_roles` (`list`). Stores the row ids of [DataBackend] in vectors of row roles:
#'   - `"use"`: Use in training.
#'   - `"validation"`: Do not use in training, this are (possibly unlabeled) observations
#'     which are held back unless explicitly addressed.
#'   To alter the role, use `set_row_role()`.
#' * `$target_names` (`character()`) returns all column names with `role == "target"`.
#' * `$new()` initializes a new object of class [Task].
#' * `$levels()` (`character()`) queries the distinct levels of the column `col`. Only works for `character` and `factor` columns.
#'   This function ignores the row roles, so you get all levels found in the [DataBackend].
#' * `$rbind()` extends the task with additional rows.
#' * `$select()` reduces the task, subsetting it to only the columns specified.
#' * `$replace_features()` replaces the features of the task with new data by replacing the [DataBackend].
#'   The new backend is always a [DataBackendDataTable].
#'   This operation is similar to calling `select()` and `cbind()`, but allows the garbage collector to clean
#'   up the previous backend.
#' * `$set_col_role()` sets the role for specified columns, referenced by name.
#'   If `exclusive` is `TRUE`, the referenced columns will be removed from all other roles.
#' * `$set_row_role()` sets the role for specified rows, referenced by row id.
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
NULL

#' @include mlr_reflections.R
#' @export
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

     groups = function() {
       groups = self$col_roles$group
       if (length(groups) == 0L)
         return(NULL)
       setnames(self$backend$data(self$row_roles$use, c(self$backend$primary_key, groups)), groups, "groups")
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
      hash(list(private$.id, self$backend$hash, self$row_roles,
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

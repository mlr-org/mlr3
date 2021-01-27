#' @title Task Class
#'
#' @include mlr_reflections.R
#'
#' @description
#' This is the abstract base class for [TaskSupervised] and [TaskUnsupervised].
#' [TaskClassif] and [TaskRegr] inherit from [TaskSupervised].
#' More supervised tasks are implemented in \CRANpkg{mlr3proba}, unsupervised cluster tasks
#' in package \CRANpkg{mlr3cluster}.
#'
#' Tasks serve two purposes:
#'
#' 1. Tasks wrap a [DataBackend], an object to transparently interface different data storage types.
#' 2. Tasks store meta-information, such as the role of the individual columns in the [DataBackend].
#'    For example, for a classification task a single column must be marked as target column, and others as features.
#'
#' Predefined (toy) tasks are stored in the [dictionary][mlr3misc::Dictionary] [mlr_tasks],
#' e.g. [`iris`][mlr_tasks_iris] or [`boston_housing`][mlr_tasks_boston_housing].
#' More toy tasks can be found in the dictionary after loading \CRANpkg{mlr3data}.
#'
#' @template param_id
#' @template param_backend
#' @template param_task_type
#' @template param_rows
#' @template param_cols
#' @template param_data_format
#' @template param_extra_args
#'
#' @section S3 methods:
#' * `as.data.table(t)`\cr
#'   [Task] -> [data.table::data.table()]\cr
#'   Returns the complete data as [data.table::data.table()].
#'
#' @section Task mutators:
#' The following methods change the task in-place:
#' * Any modification of the lists `$col_roles` or `$row_roles`.
#'   This provides a different "view" on the data without altering the data itself.
#' * Modification of column or row roles via `$set_col_roles()` or `$set_row_roles()`, respectively.
#' * `$filter()` and `$select()` subset the set of active rows or features in `$row_roles` or `$col_roles`, respectively.
#'   This provides a different "view" on the data without altering the data itself.
#' * `rbind()` and `cbind()` change the task in-place by binding rows or columns to the data, but without modifying the original [DataBackend].
#'   Instead, the methods first create a new [DataBackendDataTable] from the provided new data, and then
#'   merge both backends into an abstract [DataBackend] which merges the results on-demand.
#' * `rename()` wraps the [DataBackend] of the Task in an additional [DataBackend] which deals with the renaming. Also updates `$col_roles` and `$col_info`.
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
    #' @template field_id
    id = NULL,

    #' @template field_task_type
    task_type = NULL,

    #' @field backend ([DataBackend])\cr
    #' Abstract interface to the data of the task.
    backend = NULL,

    #' @field col_info ([data.table::data.table()])\cr
    #' Table with with 3 columns:
    #' - `"id"` (`character()`) stores the name of the column.
    #' - `"type"` (`character()`) holds the storage type of the variable, e.g. `integer`, `numeric` or `character`.
    #'   See [mlr_reflections$task_feature_types][mlr_reflections] for a complete list of allowed types.
    #' - `"levels"` stores a vector of distinct values (levels) for ordered and unordered factor variables.
    col_info = NULL,

    #' @template field_man
    man = NA_character_,

    #' @field extra_args (named `list()`)\cr
    #' Additional arguments set during construction.
    #' Required for [convert_task()].
    extra_args = NULL,

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' Note that this object is typically constructed via a derived classes, e.g. [TaskClassif] or [TaskRegr].
    initialize = function(id, task_type, backend, extra_args = list()) {
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

      cn = self$col_info$id # note: this sorts the columns!
      rn = self$backend$rownames
      private$.row_roles = list(use = rn, validation = integer())
      private$.col_roles = named_list(mlr_reflections$task_col_roles[[task_type]], character())
      private$.col_roles$feature = setdiff(cn, self$backend$primary_key)
      self$extra_args = assert_list(extra_args, names = "unique")
    },

    #' @description
    #' Opens the corresponding help page referenced by field `$man`.
    help = function() {
      open_help(self$man)
    },

    #' @description
    #' Helper for print outputs.
    format = function() {
      sprintf("<%s:%s>", class(self)[1L], self$id)
    },

    #' @description
    #' Printer.
    #' @param ... (ignored).
    print = function(...) {
      task_print(self)
    },

    #' @description
    #' Returns a slice of the data from the [DataBackend] in the data format specified by `data_format`.
    #' Rows default to observations with role `"use"`, and
    #' columns default to features with roles `"target"` or `"feature"`.
    #' If `rows` or `cols` are specified which do not exist in the [DataBackend],
    #' an exception is raised.
    #'
    #' Rows and columns are returned in the order specified via the arguments `rows` and `cols`.
    #' If `rows` is `NULL`, rows are returned in the order of `task$row_ids`.
    #' If `cols` is `NULL`, the column order defaults to
    #' `c(task$target_names, task$feature_names)`.
    #' Note that it is recommended to **not** rely on the order of columns, and instead always
    #' address columns with their respective column name.
    #'
    #' @param ordered (`logical(1)`)\cr
    #'   If `TRUE` (default), data is ordered according to the columns with column role `"order"`.
    #'
    #' @return Depending on the [DataBackend], but usually a [data.table::data.table()].
    data = function(rows = NULL, cols = NULL, data_format = "data.table", ordered = TRUE) {
      assert_has_backend(self)
      assert_choice(data_format, self$data_formats)
      assert_flag(ordered)

      row_roles = self$row_roles
      col_roles = self$col_roles

      if (is.null(rows)) {
        rows = row_roles$use
      } else {
        assert_subset(rows, self$backend$rownames)
        if (is.double(rows)) {
          rows = as.integer(rows)
        }
      }

      if (is.null(cols)) {
        query_cols = cols = c(col_roles$target, col_roles$feature)
      } else {
        assert_subset(cols, self$backend$colnames)
        query_cols = cols
      }

      reorder_rows = length(col_roles$order) > 0L && ordered
      if (reorder_rows) {
        if (data_format != "data.table") {
          stopf("Ordering only supported for data_format 'data.table'")
        }
        query_cols = union(query_cols, col_roles$order)
      }

      data = self$backend$data(rows = rows, cols = query_cols, data_format = data_format)

      if (length(query_cols) && nrow(data) != length(rows)) {
        stopf("DataBackend did not return the queried rows correctly: %i requested, %i received", length(rows), nrow(data))
      }

      if (length(rows) && ncol(data) != length(query_cols)) {
        stopf("DataBackend did not return the queried cols correctly: %i requested, %i received", length(cols), ncol(data))
      }

      if (reorder_rows) {
        setorderv(data, col_roles$order)[]
        data = remove_named(data, setdiff(col_roles$order, cols))
      }

      return(data)
    },

    #' @description
    #' Constructs a [formula()], e.g. `[target] ~ [feature_1] + [feature_2] + ... + [feature_k]`,
    #' using the features provided in argument `rhs` (defaults to all columns with role `"feature"`, symbolized by `"."`).
    #' @param rhs (`character(1)`)\cr
    #'   Right hand side of the formula. Defaults to `"."` (all features of the task).
    #' @return [formula()].
    formula = function(rhs = ".") {
      formulate(self$target_names, rhs)
    },

    #' @description
    #' Get the first `n` observations with role `"use"` of all columns with role `"target"` or `"feature"`.
    #'
    #' @param n (`integer(1)`).
    #' @return [data.table::data.table()] with `n` rows.
    head = function(n = 6L) {
      assert_has_backend(self)
      assert_count(n)
      ids = head(private$.row_roles$use, n)
      cols = c(private$.col_roles$target, private$.col_roles$feature)
      self$data(rows = ids, cols = cols)
    },

    #' @description
    #' Returns the distinct values for columns referenced in `cols` with storage type "factor" or "ordered".
    #' Argument `cols` defaults to all such columns with role `"target"` or `"feature"`.
    #'
    #' Note that this function ignores the row roles, it returns all levels available in the [DataBackend].
    #' To update the stored level information, e.g. after subsetting a task with `$filter()`, call `$droplevels()`.
    #'
    #' @return named `list()`.
    levels = function(cols = NULL) {
      if (is.null(cols)) {
        cols = unlist(private$.col_roles[c("target", "feature")], use.names = FALSE)
        cols = self$col_info[get("id") %in% cols & get("type") %in% c("factor", "ordered"), "id", with = FALSE][[1L]]
      } else {
        assert_subset(cols, self$col_info$id)
      }

      set_names(self$col_info[list(cols), "levels", on = "id", with = FALSE][[1L]], cols)
    },

    #' @description
    #' Returns the number of missing observations for columns referenced in `cols`.
    #' Considers only active rows with row role `"use"`.
    #' Argument `cols` defaults to all columns with role "target" or "feature".
    #'
    #' @return Named `integer()`.
    missings = function(cols = NULL) {
      assert_has_backend(self)

      if (is.null(cols)) {
        cols = unlist(private$.col_roles[c("target", "feature")], use.names = FALSE)
      } else {
        assert_subset(cols, self$col_info$id)
      }

      self$backend$missings(self$row_ids, cols = cols)
    },

    #' @description
    #' Subsets the task, keeping only the rows specified via row ids `rows`.
    #'
    #' This operation mutates the task in-place.
    #' See the section on task mutators for more information.
    #'
    #' @return
    #' Returns the object itself, but modified **by reference**.
    #' You need to explicitly `$clone()` the object beforehand if you want to keeps
    #' the object in its previous state.
    filter = function(rows) {
      assert_has_backend(self)
      rows = assert_row_ids(rows)
      private$.row_roles$use = intersect(private$.row_roles$use, rows)
      invisible(self)
    },

    #' @description
    #' Subsets the task, keeping only the features specified via column names `cols`.
    #' Note that you cannot deselect the target column, for obvious reasons.
    #'
    #' This operation mutates the task in-place.
    #' See the section on task mutators for more information.
    #'
    #' @return
    #' Returns the object itself, but modified **by reference**.
    #' You need to explicitly `$clone()` the object beforehand if you want to keeps
    #' the object in its previous state.
    select = function(cols) {
      assert_has_backend(self)
      assert_subset(cols, private$.col_roles$feature)
      private$.col_roles$feature = intersect(private$.col_roles$feature, cols)
      invisible(self)
    },

    #' @description
    #' Adds additional rows to the [DataBackend] stored in `$backend`.
    #' New row ids are automatically created, unless `data` has a column whose name matches
    #' the primary key of the [DataBackend] (`task$backend$primary_key`).
    #' In case of name clashes of row ids, rows in `data` have higher precedence
    #' and virtually overwrite the rows in the [DataBackend].
    #'
    #' All columns with the roles `"target"`, `"feature"`, `"weight"`, `"group"`, `"stratum"`, and `"order"` must be present in `data`.
    #' Columns only present in `data` but not in the [DataBackend] of `task` will be discarded.
    #'
    #' This operation mutates the task in-place.
    #' See the section on task mutators for more information.
    #'
    #' @param data (`data.frame()`).
    #'
    #' @return
    #' Returns the object itself, but modified **by reference**.
    #' You need to explicitly `$clone()` the object beforehand if you want to keeps
    #' the object in its previous state.
    rbind = function(data) {
      assert_has_backend(self)

      pk = self$backend$primary_key
      rn = self$backend$rownames
      pk_in_backend = TRUE
      type_check = TRUE

      if (is.data.frame(data)) {
        pk_in_backend = pk %in% names(data)
        type_check = FALSE # done by auto-converter

        keep_cols = intersect(names(data), self$backend$colnames)
        if (length(keep_cols) == pk_in_backend || nrow(data) == 0L) {
          return(invisible(self))
        }

        if (!pk_in_backend) {
          start = if (length(rn)) max(rn) + 1L else 1L
          pk = seq(from = start, to = start + nrow(data) - 1L)
        }

        ci = self$col_info[list(keep_cols), on = "id"]
        data = do.call(data.table, Map(auto_convert,
            value = as.list(data)[ci$id],
            id = ci$id, type = ci$type, levels = ci$levels))

        data = as_data_backend(data, primary_key = pk)
      } else {
        assert_backend(data)
        if (data$ncol <= 1L || data$nrow == 0L) {
          return(invisible(self))
        }
      }

      if (pk_in_backend && any(data$rownames %in% self$backend$rownames)) {
        stopf("Cannot rbind data to task '%s', duplicated row ids", self$id)
      }

      # columns with these roles must be present in data
      mandatory_roles = c("target", "feature", "weight")
      mandatory_cols = unlist(self$col_roles[mandatory_roles], use.names = FALSE)
      missing_cols = setdiff(mandatory_cols, data$colnames)
      if (length(missing_cols)) {
        stopf("Cannot rbind data to task '%s', missing the following mandatory columns: %s", self$id, str_collapse(missing_cols))
      }

      # merge col infos
      tab = merge(self$col_info, col_info(data), by = "id",
        all.x = TRUE, all.y = FALSE, suffixes = c("", "_y"), sort = TRUE)
      levels = levels_y = type = type_y = NULL

      # type check
      if (type_check) {
        ii = head(tab[type != type_y, which = TRUE], 1L)
        if (length(ii)) {
          stopf("Cannot rbind to task: Types do not match for column: %s (%s != %s)", tab$id[ii], tab$type[ii], tab$type_y[ii])
        }
      }

      # merge factor levels
      vunion = function(x, y) Map(union, x, y)
      tab[type %in% c("factor", "ordered"), levels := list(vunion(levels, levels_y))]
      tab[, c("type_y", "levels_y") := list(NULL, NULL)]

      # everything looks good, modify task
      self$backend = DataBackendRbind$new(self$backend, data)
      self$col_info = tab
      self$row_roles$use = c(self$row_roles$use, data$rownames)

      invisible(self)
    },

    #' @description
    #'
    #' Adds additional columns to the [DataBackend] stored in `$backend`.
    #'
    #' The row ids must be provided as column in `data` (with column name matching the primary key name of the [DataBackend]).
    #' If this column is missing, it is assumed that the rows are exactly in the order of `$row_ids`.
    #' In case of name clashes of column names in `data` and [DataBackend], columns in `data` have higher precedence
    #' and virtually overwrite the columns in the [DataBackend].
    #'
    #' This operation mutates the task in-place.
    #' See the section on task mutators for more information.
    #' @param data (`data.frame()`).
    cbind = function(data) {
      assert_has_backend(self)
      pk = self$backend$primary_key

      if (is.data.frame(data)) {
        # binding data with 0 rows is explicitly allowed
        if (ncol(data) == 0L) {
          return(invisible(self))
        }

        row_ids = if (pk %in% names(data)) pk else self$row_ids
        data = as_data_backend(data, primary_key = row_ids)
      } else {
        assert_backend(data)
        if (data$ncol <= 1L) {
          return(invisible(self))
        }
      }

      assert_set_equal(self$row_ids, data$rownames)
      ci = col_info(data)

      # update col info
      self$col_info = ujoin(self$col_info, ci, key = "id")
      self$col_info = rbind(self$col_info, ci[!list(self$col_info), on = "id"])
      setkeyv(self$col_info, "id")

      # add new features
      self$col_roles$feature = union(self$col_roles$feature, setdiff(data$colnames, c(pk, self$col_roles$target)))

      # update backend
      self$backend = DataBackendCbind$new(self$backend, data)

      invisible(self)
    },


    #' @description
    #' Renames columns by mapping column names in `old` to new column names in `new` (element-wise).
    #'
    #' This operation mutates the task in-place.
    #' See the section on task mutators for more information.
    #'
    #' @param old (`character()`)\cr
    #'   Old names.
    #'
    #' @param new (`character()`)\cr
    #'   New names.
    #'
    #' @return
    #' Returns the object itself, but modified **by reference**.
    #' You need to explicitly `$clone()` the object beforehand if you want to keeps
    #' the object in its previous state.
    rename = function(old, new) {
      assert_has_backend(self)
      self$backend = DataBackendRename$new(self$backend, old, new)
      setkeyv(self$col_info[old, ("id") := new, on = "id"], "id")
      self$col_roles = map(self$col_roles, map_values, old = old, new = new)
      invisible(self)
    },

    #' @description
    #' Modifies the roles in `$row_roles` **in-place**.
    #'
    #' @param rows (`integer()`)\cr
    #'   Row ids for which to change the roles for.
    #' @param roles (`character()`)\cr
    #'   Exclusively set rows to the specified `roles` (remove from other roles).
    #' @param add_to (`character()`)\cr
    #'   Add rows with row ids `rows` to roles specified in `add_to`.
    #'   Rows keep their previous roles.
    #' @param remove_from (`character()`)\cr
    #'   Remove rows with row ids `rows` from roles specified in `remove_from`.
    #'   Other row roles are preserved.
    #'
    #' @details
    #' Roles are first set exclusively (argument `roles`), then added (argument `add_to`) and finally
    #' removed (argument `remove_from`) from different roles.
    #'
    #' @return
    #' Returns the object itself, but modified **by reference**.
    #' You need to explicitly `$clone()` the object beforehand if you want to keeps
    #' the object in its previous state.
    set_row_roles = function(rows, roles = NULL, add_to = NULL, remove_from = NULL) {
      assert_has_backend(self)
      assert_subset(rows, self$backend$rownames)
      private$.row_roles = task_set_roles(private$.row_roles, rows, roles, add_to, remove_from)
      invisible(self)
    },

    #' @description
    #' Modifies the roles in `$col_roles` **in-place**.
    #'
    #' @param cols (`character()`)\cr
    #'   Column names for which to change the roles for.
    #' @param roles (`character()`)\cr
    #'   Exclusively set columns to the specified `roles` (remove from other roles).
    #' @param add_to (`character()`)\cr
    #'   Add columns with column names `cols` to roles specified in `add_to`.
    #'   Columns keep their previous roles.
    #' @param remove_from (`character()`)\cr
    #'   Remove columns with columns names `cols` from roles specified in `remove_from`.
    #'   Other column roles are preserved.
    #'
    #' @details
    #' Roles are first set exclusively (argument `roles`), then added (argument `add_to`) and finally
    #' removed (argument `remove_from`) from different roles.
    #'
    #' @return
    #' Returns the object itself, but modified **by reference**.
    #' You need to explicitly `$clone()` the object beforehand if you want to keeps
    #' the object in its previous state.
    set_col_roles = function(cols, roles = NULL, add_to = NULL, remove_from = NULL) {
      assert_has_backend(self)
      assert_subset(cols, self$backend$colnames)
      new_roles = task_set_roles(private$.col_roles, cols, roles, add_to, remove_from)
      private$.col_roles = task_check_col_roles(self, new_roles)
      invisible(self)
    },

    #' @description
    #' Updates the cache of stored factor levels, removing all levels not present in the current set of active rows.
    #' `cols` defaults to all columns with storage type "factor" or "ordered".
    #' @return Modified `self`.
    droplevels = function(cols = NULL) {
      assert_has_backend(self)
      tab = self$col_info[get("type") %in% c("factor", "ordered"), c("id", "levels"), with = FALSE]
      if (!is.null(cols)) {
        tab = tab[list(cols), on = "id", nomatch = NULL]
      }

      # query new levels
      new_levels = self$backend$distinct(rows = self$row_ids, cols = tab$id)

      # update levels column with new levels:
      # * first "known" levels in the original order of levels,
      # * then new levels order of occurrence in new_levels
      tab$levels = Map(function(x, y) c(intersect(x, y), setdiff(y, x)),
        x = tab$levels, y = new_levels)

      self$col_info = ujoin(self$col_info, tab, key = "id")
      invisible(self)
    }
  ),

  active = list(
    #' @template field_hash
    hash = function(rhs) {
      private$.hash %??% hash(
        class(self), self$id, self$backend$hash, self$col_info,
        private$.row_roles, private$.col_roles, private$.properties
      )
    },

    #' @field row_ids (`integer()`)\cr
    #' Returns the row ids of the [DataBackend] for observations with role "use".
    row_ids = function(rhs) {
      assert_ro_binding(rhs)
      private$.row_roles$use
    },

    #' @field row_names ([data.table::data.table()])\cr
    #' Returns a table with two columns:
    #'
    #' * `"row_id"` (`integer()`), and
    #' * `"row_name"` (`character()`).
    row_names = function(rhs) {
      assert_ro_binding(rhs)
      nn = self$col_roles$name
      if (length(nn) == 0L) {
        return(NULL)
      }
      setnames(self$backend$data(rows = self$row_ids, cols = c(self$backend$primary_key, nn)),
        c("row_id", "row_name"))
    },

    #' @field feature_names (`character()`)\cr
    #' Returns all column names with `role == "feature"`.
    #'
    #' Note that this vector determines the default order of columns for `task$data(cols = NULL, ...)`.
    #' However, it is recommended to **not** rely on the order of columns, but instead always
    #' address columns by their name. The default order is not well defined after some
    #' operations, e.g. after `task$cbind()` or after processing via \CRANpkg{mlr3pipelines}.
    feature_names = function(rhs) {
      assert_ro_binding(rhs)
      private$.col_roles$feature
    },

    #' @field target_names (`character()`)\cr
    #' Returns all column names with role "target".
    target_names = function(rhs) {
      assert_ro_binding(rhs)
      private$.col_roles$target
    },

    #' @field properties (`character()`)\cr
    #' Set of task properties.
    #' Possible properties are are stored in [mlr_reflections$task_properties][mlr_reflections].
    #' The following properties are currently standardized and understood by tasks in \CRANpkg{mlr3}:
    #'
    #' * `"strata"`: The task is resampled using one or more stratification variables (role `"stratum"`).
    #' * `"groups"`: The task comes with grouping/blocking information (role `"group"`).
    #' * `"weights"`: The task comes with observation weights (role `"weight"`).
    #'
    #' Note that above listed properties are calculated from the `$col_roles` and may not be set explicitly.
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

    #' @field row_roles (named `list()`)\cr
    #' Each row (observation) can have an arbitrary number of roles in the learning task:
    #'
    #' - `"use"`: Use in train / predict / resampling.
    #' - `"validation"`: Observations are hold back unless explicitly requested.
    #'   Can be used as truly independent test set.
    #'
    #' `row_roles` is a named list whose elements are named by row role and each element is an `integer()` vector of row ids.
    #' To alter the roles, just modify the list, e.g. with  \R's set functions ([intersect()], [setdiff()], [union()], \ldots).
    row_roles = function(rhs) {
      if (missing(rhs)) {
        return(private$.row_roles)
      }

      assert_has_backend(self)
      assert_list(rhs, .var.name = "row_roles")
      assert_names(names(rhs), "unique", permutation.of = mlr_reflections$task_row_roles, .var.name = "names of row_roles")
      rhs = map(rhs, assert_row_ids, .var.name = "elements of row_roles")

      private$.row_roles = rhs
    },

    #' @field col_roles (named `list()`)\cr
    #' Each column (feature) can have an arbitrary number of the following roles:
    #'
    #' * `"feature"`: Regular feature used in the model fitting process.
    #' * `"target"`: Target variable.
    #' * `"name"`: Row names / observation labels. To be used in plots. Can be queried with `$row_names`.
    #' * `"order"`: Data returned by `$data()` is ordered by this column (or these columns).
    #' * `"group"`: During resampling, observations with the same value of the variable with role "group" are marked as "belonging together".
    #'   For each resampling iteration, observations of the same group will be exclusively assigned to be either in the training set or in the test set.
    #'   Note that only up to one column may have this role.
    #' * `"stratum"`: Stratification variables. Multiple discrete columns may have this role.
    #' * `"weight"`: Observation weights. Only up to one column (assumed to be discrete) may have this role.
    #' * `"uri"`: URI pointing to an external resource, e.g., images on the file system.
    #'
    #' `col_roles` is a named list whose elements are named by column role and each element is a `character()` vector of column names.
    #'   To alter the roles, just modify the list, e.g. with \R's set functions ([intersect()], [setdiff()], [union()], \ldots).
    col_roles = function(rhs) {
      if (missing(rhs)) {
        return(private$.col_roles)
      }

      assert_has_backend(self)
      qassertr(rhs, "S[1,]", .var.name = "col_roles")
      assert_names(names(rhs), "unique", must.include = mlr_reflections$task_col_roles[[self$task_type]], .var.name = "names of col_roles")
      assert_subset(unlist(rhs, use.names = FALSE), setdiff(self$col_info$id, self$backend$primary_key), .var.name = "elements of col_roles")

      private$.col_roles = task_check_col_roles(self, rhs)
    },

    #' @field nrow (`integer(1)`)\cr
    #' Returns the total number of rows with role "use".
    nrow = function(rhs) {
      assert_ro_binding(rhs)
      length(private$.row_roles$use)
    },

    #' @field ncol (`integer(1)`)\cr
    #' Returns the total number of columns with role "target" or "feature".
    ncol = function(rhs) {
      assert_ro_binding(rhs)
      length(private$.col_roles$feature) + length(private$.col_roles$target)
    },

    #' @field feature_types ([data.table::data.table()])\cr
    #' Returns a table with columns `id` and `type` where `id` are the column names of "active"
    #' features of the task and `type` is the storage type.
    feature_types = function(rhs) {
      assert_ro_binding(rhs)
      setkeyv(self$col_info[list(private$.col_roles$feature), c("id", "type"), on = "id"], "id")
    },

    #' @field data_formats `character()`\cr
    #'   Vector of supported data output formats.
    #'   A specific format can be chosen in the `$data()` method.
    data_formats = function(rhs) {
      assert_ro_binding(rhs)
      self$backend$data_formats %??% character()
    },

    #' @field strata ([data.table::data.table()])\cr
    #' If the task has columns designated with role `"stratum"`, returns a table with one subpopulation per row and two columns:
    #'
    #' * `N` (`integer()`) with the number of observations in the subpopulation, and
    #' * `row_id` (list of `integer()`) as list column with the row ids in the respective subpopulation.
    #' Returns `NULL` if there are is no stratification variable.
    #' See [Resampling] for more information on stratification.
    strata = function(rhs) {
      assert_has_backend(self)
      assert_ro_binding(rhs)
      cols = private$.col_roles$stratum
      if (length(cols) == 0L) {
        return(NULL)
      }

      row_ids = self$row_ids
      tab = self$backend$data(rows = row_ids, cols = cols)
      tab$..row_id = row_ids
      tab = tab[, list(..N = .N, ..row_id = list(.SD$..row_id)), by = cols, .SDcols = "..row_id"][, (cols) := NULL]
      setnames(tab, c("..N", "..row_id"), c("N", "row_id"))[]
    },


    #' @field groups ([data.table::data.table()])\cr
    #' If the task has a column with designated role `"group"`, a table with two columns:
    #'
    #' * `row_id` (`integer()`), and
    #' * grouping variable `group` (`vector()`).
    #'
    #' Returns `NULL` if there are is no grouping column.
    #' See [Resampling] for more information on grouping.
    groups = function(rhs) {
      assert_has_backend(self)
      assert_ro_binding(rhs)
      group_cols = private$.col_roles$group
      if (length(group_cols) == 0L) {
        return(NULL)
      }
      data = self$backend$data(private$.row_roles$use, c(self$backend$primary_key, group_cols))
      setnames(data, c("row_id", "group"))[]
    },

    #' @field order ([data.table::data.table()])\cr
    #' If the task has at least one column with designated role `"order"`, a table with two columns:
    #'
    #' * `row_id` (`integer()`), and
    #' * ordering vector `order` (`integer()`).
    #'
    #' Returns `NULL` if there are is no order column.
    order = function(rhs) {
      assert_has_backend(self)
      assert_ro_binding(rhs)

      order_cols = private$.col_roles$order
      if (length(order_cols) == 0L) {
        return(NULL)
      }

      data = self$backend$data(private$.row_roles$use, order_cols)
      data.table(row_id = private$.row_roles$use, order = do.call(order, data))
    },

    #' @field weights ([data.table::data.table()])\cr
    #' If the task has a column with designated role `"weight"`, a table with two columns:
    #'
    #' * `row_id` (`integer()`), and
    #' * observation weights `weight` (`numeric()`).
    #'
    #' Returns `NULL` if there are is no weight column.
    weights = function(rhs) {
      assert_has_backend(self)
      assert_ro_binding(rhs)
      weight_cols = private$.col_roles$weight
      if (length(weight_cols) == 0L) {
        return(NULL)
      }
      data = self$backend$data(private$.row_roles$use, c(self$backend$primary_key, weight_cols))
      setnames(data, c("row_id", "weight"))[]
    },

    #' @field uris ([data.table::data.table()])\cr
    #' If the task has a column with designated role `"uri"`, a table with two columns:
    #'
    #' * `row_id` (`integer()`), and
    #' * `uri` (`character()`).
    #'
    #' Returns `NULL` if there are is no uri column.
    uris = function(rhs) {
      assert_has_backend(self)
      assert_ro_binding(rhs)
      uri_col = private$.col_roles$uri
      if (length(uri_col) == 0L) {
        return(NULL)
      }
      data = self$backend$data(private$.row_roles$use, c(self$backend$primary_key, uri_col))
      setnames(data, c("row_id", "uri"))[]
    }
  ),

  private = list(
    .properties = NULL,
    .col_roles = NULL,
    .row_roles = NULL,
    .hash = NULL,

    deep_clone = function(name, value) {
      # NB: DataBackends are never copied!
      # TODO: check if we can assume col_info to be read-only
      if (name == "col_info") copy(value) else value
    }
  )
)

task_data = function(self, rows = NULL, cols = NULL, data_format = "data.table", ordered = TRUE) {
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

task_set_roles = function(li, cols, roles = NULL, add_to = NULL, remove_from = NULL) {
  if (!is.null(roles)) {
    assert_subset(roles, names(li))
    for (role in roles) {
      li[[role]] = union(li[[role]], cols)
    }
    for (role in setdiff(names(li), roles)) {
      li[[role]] = setdiff(li[[role]], cols)
    }
  }

  if (!is.null(add_to)) {
    assert_subset(add_to, names(li))
    for (role in add_to) {
      li[[role]] = union(li[[role]], cols)
    }
  }

  if (!is.null(remove_from)) {
    assert_subset(remove_from, names(li))
    for (role in remove_from) {
      li[[role]] = setdiff(li[[role]], cols)
    }
  }

  li
}

task_check_col_roles = function(self, new_roles) {
  for (role in c("group", "weight", "name")) {
    if (length(new_roles[[role]]) > 1L) {
      stopf("There may only be up to one column with role '%s'", role)
    }
  }

  if (inherits(self, "TaskSupervised")) {
    if (length(new_roles$target) == 0L) {
      stopf("Supervised tasks need at least one target column")
    }
  } else if (inherits(self, "TaskUnsupervised")) {
    if (length(new_roles$target) != 0L) {
      stopf("Unsupervised tasks may not have a target column")
    }
  }

  new_roles
}

# collect column information of a backend.
# This currently includes:
# * storage type
# * levels (for character / factor / ordered), but not for the primary key column
col_info = function(x, ...) {
  UseMethod("col_info")
}

#' @export
col_info.data.table = function(x, primary_key = character(), ...) { # nolint
  types = map_chr(x, function(x) class(x)[1L])
  discrete = setdiff(names(types)[types %in% c("factor", "ordered")], primary_key)
  levels = insert_named(named_list(names(types)), lapply(x[, discrete, with = FALSE], distinct_values, drop = FALSE))
  data.table(id = names(types), type = unname(types), levels = levels, key = "id")
}

#' @export
col_info.DataBackend = function(x, ...) { # nolint
  types = map_chr(x$head(1L), function(x) class(x)[1L])
  discrete = setdiff(names(types)[types %in% c("factor", "ordered")], x$primary_key)
  levels = insert_named(named_list(names(types)), x$distinct(rows = NULL, cols = discrete))
  data.table(id = names(types), type = unname(types), levels = levels, key = "id")
}

#' @export
as.data.table.Task = function(x, ...) { # nolint
  x$head(x$nrow)
}

task_rm_backend = function(task) {
  # fix task hash
  ee = get_private(task)
  ee$.hash = force(task$hash)

  # NULL backend
  task$backend = NULL

  task
}


#' @export
rd_info.Task = function(obj, section) { # nolint
  c("",
    sprintf("* Task type: %s", rd_format_string(obj$task_type)),
    sprintf("* Dimensions: %ix%i", obj$nrow, obj$ncol),
    sprintf("* Properties: %s", rd_format_string(obj$properties)),
    sprintf("* Has Missings: `%s`", any(obj$missings() > 0L)),
    sprintf("* Target: %s", rd_format_string(obj$target_names)),
    sprintf("* Features: %s", rd_format_string(obj$feature_names))
  )
}

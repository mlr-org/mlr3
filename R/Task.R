#' @title Task Class
#'
#' @include mlr_reflections.R
#' @include warn_deprecated.R
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
#' e.g. [`penguins`][mlr_tasks_penguins] or [`california_housing`][mlr_tasks_california_housing].
#' More toy tasks can be found in the dictionary after loading \CRANpkg{mlr3data}.
#'
#' @template param_id
#' @template param_backend
#' @template param_task_type
#' @template param_rows
#' @template param_cols
#' @template param_label
#' @template param_extra_args
#'
#' @section S3 methods:
#' * `as.data.table(t)`\cr
#'   [Task] -> [data.table::data.table()]\cr
#'   Returns the complete data as [data.table::data.table()].
#' * `head(t)`\cr
#'   Calls [head()] on the task's data.
#' * `summary(t)`\cr
#'   Calls [summary()] on the task's data.
#'
#'
#' @section Task mutators:
#' The following methods change the task in-place:
#' * Any modification of the lists `$col_roles` or `$row_roles`.
#'   This provides a different "view" on the data without altering the data itself.
#'   This may affects, e.g., `$data`, `$nrow`, `$ncol`, `n_features`, `row_ids`, and `$feature_names`.
#'   Altering `$col_roles` may affect, e.g., `$data`, `$ncol`, `$n_features`, and `$feature_names`.
#'   Altering `$row_roles` may affect, e.g., `$data`, `$nrow`, and `$row_ids`.
#' * Modification of column or row roles via `$set_col_roles()` or `$set_row_roles()`, respectively.
#'   They are an alternative to directly accessing `$col_roles` or `$row_roles`, with the same side effects.
#' * `$select()` and `$filter()` subset the set of active features or rows in `$col_roles` or `$row_roles`, respectively.
#' * `$cbind()` and `$rbind()` change the task in-place by binding new columns or rows to the data.
#' * `$rename()` changes column names.
#' * `$set_levels()` and `$droplevels()` update the field `$col_info()` to automatically repair factor levels while querying data with `$data()`.
#' * `$materialize_view()` creates a new [DataBackendDataTable] which keeps only the data in the currently active view
#'   possibly freeing some memory consumed by the [DataBackend] stored in the `Task`.
#'
#' @template seealso_task
#' @concept Task
#' @export
#' @examples
#' # We use the inherited class TaskClassif here,
#' # because the base class `Task` is not intended for direct use
#' task = TaskClassif$new("penguings", palmerpenguins::penguins, target = "species")
#'
#' task$nrow
#' task$ncol
#' task$feature_names
#' task$formula()
#'
#' # de-select "year"
#' task$select(setdiff(task$feature_names, "year"))
#'
#' task$feature_names
#'
#' # Add new column "foo"
#' task$cbind(data.frame(foo = 1:344))
#' head(task)
Task = R6Class("Task",
  public = list(
    #' @template field_label
    label = NA_character_,

    #' @template field_task_type
    task_type = NULL,

    #' @field backend ([DataBackend])\cr
    #' Abstract interface to the data of the task.
    backend = NULL,

    #' @field col_info ([data.table::data.table()])\cr
    #' Table with with 4 columns, mainly for internal purposes:
    #' - `"id"` (`character()`) stores the name of the column.
    #' - `"type"` (`character()`) holds the storage type of the variable, e.g. `integer`, `numeric` or `character`.
    #'   See [mlr_reflections$task_feature_types][mlr_reflections] for a complete list of allowed types.
    #' - `"levels"` (`list()`) stores a vector of distinct values (levels) for ordered and unordered factor variables.
    #' - `"label"` (`character()`) stores a vector of prettier, formated column names.
    #' - `"fix_factor_levels"` (`logical()`) stores flags which determine if the levels of the respective variable
    #'   need to be reordered after querying the data from the [DataBackend].
    #'
    #' Note that all columns of the [DataBackend], also columns which are not selected or have any role, are listed
    #' in this table.
    col_info = NULL,

    #' @template field_man
    man = NA_character_,

    #' @field extra_args (named `list()`)\cr
    #' Additional arguments set during construction.
    #' Required for [convert_task()].
    extra_args = NULL,

    #' @field mlr3_version (`package_version`)\cr
    #' Package version of `mlr3` used to create the task.
    mlr3_version = NULL,

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' Note that this object is typically constructed via a derived classes, e.g. [TaskClassif] or [TaskRegr].
    initialize = function(id, task_type, backend, label = NA_character_, extra_args = list()) {
      private$.id = assert_string(id, min.chars = 1L)
      self$label = assert_string(label, na.ok = TRUE)
      self$task_type = assert_choice(task_type, mlr_reflections$task_types$type)
      if (!inherits(backend, "DataBackend")) {
        self$backend = as_data_backend(backend)
      } else {
        self$backend = assert_backend(backend)
      }

      cn = self$backend$colnames
      rn = self$backend$rownames

      assert_names(cn, "unique", .var.name = "column names")
      if (any(grepl("%", cn, fixed = TRUE))) {
        error_input("Column names may not contain special character '%%'")
      }

      self$col_info = col_info(self$backend)
      self$col_info$label = NA_character_
      self$col_info$fix_factor_levels = FALSE

      assert_subset(self$col_info$type, mlr_reflections$task_feature_types, .var.name = "feature types")
      pmap(self$col_info,
        function(id, levels, ...) {
          assert_character(levels, any.missing = FALSE, min.len = 1L, null.ok = TRUE,
            .var.name = sprintf("levels of '%s'", id))
        }
      )

      cn = self$col_info$id # note: this sorts the columns!
      private$.row_roles = list(use = rn)
      private$.col_roles = named_list(mlr_reflections$task_col_roles[[task_type]], character())
      private$.col_roles$feature = setdiff(cn, self$backend$primary_key)
      self$extra_args = assert_list(extra_args, names = "unique")
      self$mlr3_version = mlr_reflections$package_version
    },

    #' @description
    #' Deprecated.
    #'
    #' @param ratio (`numeric(1)`)\cr
    #'   The proportion of datapoints to use as validation data.
    #' @param ids (`integer()`)\cr
    #'   The row ids to use as validation data.
    #' @param remove (`logical(1)`)\cr
    #'   If `TRUE` (default), the `row_ids` are removed from the primary task's active `"use"` rows, ensuring a
    #'   disjoint split between the train and validation data.
    #'
    #' @return Modified `Self`.
    divide = function(ratio = NULL, ids = NULL, remove = TRUE) {
      .Deprecated("field $internal_valid_task")
      assert_flag(remove)
      private$.hash = NULL

      if (!xor(is.null(ratio), is.null(ids))) {
        error_input("Provide a ratio or ids to create a validation task, but not both (Task '%s').", self$id)
      }

      valid_ids = if (!is.null(ratio)) {
        assert_numeric(ratio, lower = 0, upper = 1, any.missing = FALSE)
        partition(self, ratio = 1 - ratio)$test
      } else {
        assert_row_ids(ids, null.ok = FALSE)
      }

      prev_internal_valid = private$.internal_valid_task
      if (!is.null(prev_internal_valid)) {
        lg$debug("Task %s already had an internal validation task that is being overwritten.", self$id)
        # in case something goes wrong
        on.exit({private$.internal_valid_task = prev_internal_valid}, add = TRUE)
        private$.internal_valid_task = NULL
      }
      private$.internal_valid_task = self$clone(deep = TRUE)
      private$.internal_valid_task$row_roles$use = valid_ids
      if (remove) {
        self$row_roles$use = setdiff(self$row_roles$use, valid_ids)
      }
      on.exit({}, add = FALSE)
      invisible(self)
    },

    #' @description
    #' Opens the corresponding help page referenced by field `$man`.
    help = function() {
      open_help(self$man)
    },

    #' @description
    #' Helper for print outputs.
    #' @param ... (ignored).
    format = function(...) {
      sprintf("<%s:%s>", class(self)[1L], self$id)
    },

    #' @description
    #' Printer.
    #' @param ... (ignored).
    print = function(...) {
      msg_h = if (is.null(self$label) || is.na(self$label)) "" else paste0(": ", self$label)
      cat_cli(cli_h1("{.cls {class(self)[1L]}} ({self$nrow}x{self$ncol}){msg_h}"))

      roles = private$.col_roles
      roles = roles[lengths(roles) > 0L]

      # print additional columns as specified in reflections
      before = mlr_reflections$task_print_col_roles$before
      iwalk(before[before %chin% names(roles)], function(role, str) {
        cat_cli(cli_li("{str}: {roles[[role]]}"))
      })

      cat_cli(cli_li("Target: {self$target_names}"))

      if (class(self)[1L] == "TaskClassif") {
        if (!is.null(self$backend)) {
          class_freqs = table(self$truth()) / self$nrow * 100
          class_freqs = class_freqs[order(-class_freqs, names(class_freqs))]  # Order by class frequency, then names
          classes = if ("twoclass" %in% self$properties) {
            sprintf("%s (positive class, %.0f%%), %s (%.0f%%)", self$positive, class_freqs[[self$positive]], self$negative, class_freqs[[self$negative]])
          } else {
            if (length(class_freqs) > 10L) {
              paste0(toString(sprintf("%s (%.0f%%)", names(class_freqs)[1:10], class_freqs[1:10])), " + ", length(class_freqs) - 10, " more")
            } else {
              toString(sprintf("%s (%.0f%%)", names(class_freqs), class_freqs))
            }
          }
        } else {
          classes = toString(self$class_names)
        }
        cat_cli(cli_li("Target classes: {classes}"))
      }

      properties = if (length(self$properties)) toString(self$properties) else "-"
      cat_cli(cli_li("Properties: {properties}"))

      types = self$feature_types

      if (nrow(types)) {
        cat_cli({
          id = type = NULL
          cli_li("Features ({nrow(types)}):")
          types = types[, list(N = .N, feats = str_collapse(id, n = 100L)), by = "type"][, "type" := translate_types(type)]
          setorderv(types, "N", order = -1L)

          ulid <- cli_ul()
          pmap(types, function(type, N, feats) {
            cli_li("{type} ({N}): {feats}")
          })
          cli_end(ulid)
        })
      }


      # print additional columns are specified in reflections
      after = mlr_reflections$task_print_col_roles$after
      iwalk(after[after %chin% names(roles)], function(role, str) {
        cat_cli(cli_li("{str}: {roles[[role]]}"))
      })

      if (!is.null(private$.internal_valid_task)) {
        cat_cli(cli_li("Validation Task: ({private$.internal_valid_task$nrow}x{private$.internal_valid_task$ncol})"))
      }

      if (!is.null(self$characteristics)) {
        cat_cli(cli_li("Characteristics: {as_short_string(self$characteristics)}"))
      }
    },

    #' @description
    #' Returns a slice of the data from the [DataBackend] as a `data.table`.
    #' Rows default to observations with role `"use"`, and columns default to features with roles `"target"` or `"feature"`.
    #' Rows must be a subset of `$row_ids`.
    #' If `rows` or `cols` are specified which do not exist in the [DataBackend], an exception is raised.
    #'
    #' Rows and columns are returned in the order specified via the arguments `rows` and `cols`.
    #' If `rows` is `NULL`, rows are returned in the order of `task$row_ids`.
    #' If `cols` is `NULL`, the column order defaults to `c(task$target_names, task$feature_names)`.
    #' Note that it is recommended to **not** rely on the order of columns, and instead always address columns with their respective column name.
    #'
    #' @param ordered (`logical(1)`)\cr
    #'   If `TRUE`, data is ordered according to the columns with column role `"order"`.
    #'
    #' @return Depending on the [DataBackend], but usually a [data.table::data.table()].
    #' @examples
    #' task = tsk("penguins")
    #' task$data(rows = 1:5, cols = c("species", "sex"))
    data = function(rows = NULL, cols = NULL, ordered = FALSE) {
      assert_has_backend(self)
      assert_flag(ordered)

      row_roles = private$.row_roles
      col_roles = private$.col_roles

      if (is.null(rows)) {
        rows = row_roles$use
      } else {
        assert_subset(rows, self$row_roles$use)
        if (is.double(rows)) {
          rows = as.integer(rows)
        }
      }

      if (is.null(cols)) {
        query_cols = cols = c(col_roles$target, col_roles$feature)
      } else {
        assert_subset(cols, self$col_info$id)
        query_cols = cols
      }

      reorder_rows = length(col_roles$order) > 0L && ordered
      if (reorder_rows) {
        query_cols = union(query_cols, col_roles$order)
      }

      data = self$backend$data(rows = rows, cols = query_cols)

      if (length(query_cols) && nrow(data) != length(rows)) {
        error_mlr3("DataBackend did not return the queried rows correctly: %i requested, %i received.
        The resampling was probably instantiated on a different task.", length(rows), nrow(data))  # TODO: more specific error necessary?
      }

      if (length(rows) && ncol(data) != length(query_cols)) {
        error_mlr3("DataBackend did not return the queried cols correctly: %i requested, %i received", length(cols), ncol(data))  # TODO: more specific error necessary?
      }

      .__i__ = self$col_info[["fix_factor_levels"]]
      if (any(.__i__)) {
        fix_factors = self$col_info[.__i__, c("id", "levels"), with = FALSE]
        if (nrow(fix_factors)) {
          # ordering is slow
          if (nrow(fix_factors) > 1L) fix_factors = fix_factors[list(names(data)), on = "id", nomatch = NULL]
          data = fix_factor_levels(data, levels = set_names(fix_factors$levels, fix_factors$id))
        }
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
    #'
    #' Note that it is currently not possible to change the formula.
    #' However, \CRANpkg{mlr3pipelines} provides a pipe operator interfacing [stats::model.matrix()] for this purpose: `"modelmatrix"`.
    #'
    #' @param rhs (`character(1)`)\cr
    #'   Right hand side of the formula. Defaults to `"."` (all features of the task).
    #' @return [formula()].
    #' @examples
    #' task = tsk("penguins")
    #' task$formula()
    formula = function(rhs = ".") {
      formulate(self$target_names, rhs)
    },

    #' @description
    #' Get the first `n` observations with role `"use"` of all columns with role `"target"` or `"feature"`.
    #'
    #' @param n (`integer(1)`).
    #' @return [data.table::data.table()] with `n` rows.
    #' @examples
    #' task = tsk("penguins")
    #' task$head(3)
    head = function(n = 6L) {
      assert_number(n, na.ok = FALSE)
      ids = head(private$.row_roles$use, n)
      self$data(rows = ids)
    },

    #' @description
    #' Returns the distinct values for columns referenced in `cols` with storage type "factor" or "ordered".
    #' Argument `cols` defaults to all such columns with role `"target"` or `"feature"`.
    #'
    #' Note that this function ignores the row roles, it returns all levels available in the [DataBackend].
    #' To update the stored level information, e.g. after subsetting a task with `$filter()`, call `$droplevels()`.
    #'
    #' @return named `list()`.
    #' @examples
    #' task = tsk("penguins")
    #' task$levels()
    levels = function(cols = NULL) {
      if (is.null(cols)) {
        cols = unlist(private$.col_roles[c("target", "feature")], use.names = FALSE)
        cols = self$col_info[get("id") %chin% cols & get("type") %chin% c("factor", "ordered"), "id", with = FALSE][[1L]]
      } else {
        assert_subset(cols, self$col_info$id)
      }

      set_names(
        fget_keys(self$col_info, cols, "levels", "id"),
        cols
      )
    },

    #' @description
    #' Returns the number of missing observations for columns referenced in `cols`.
    #' Considers only active rows with row role `"use"`.
    #' Argument `cols` defaults to all columns with role "target" or "feature".
    #'
    #' @return Named `integer()`.
    #' @examples
    #' task = tsk("penguins")
    #' task$missings()
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
    #' @examples
    #' task = tsk("penguins")
    #' task$filter(1:10)
    #' task$nrow
    filter = function(rows) {
      assert_has_backend(self)
      rows = assert_row_ids(rows)
      private$.row_roles$use = assert_subset(rows, self$row_ids_backend)
      private$.row_hash = NULL
      private$.hash = NULL
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
    #' @examples
    #' task = tsk("penguins")
    #' task$select(c("bill_length", "bill_depth"))
    #' task$feature_names
    select = function(cols) {
      assert_has_backend(self)
      assert_character(cols)
      assert_subset(cols, private$.col_roles$feature)
      private$.hash = NULL
      private$.col_hashes = NULL
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
    #' All columns roles `"target"`, `"feature"`, `"weights_learner"`, `"weights_measure"`,
    #' `"group"`, `"stratum"`, and `"order"` must be present in `data`.
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
    #' @examples
    #' task = tsk("penguins")
    #' extra = task$data(rows = 1:2)
    #' task$rbind(extra)
    rbind = function(data) {
      assert_has_backend(self)

      pk = self$backend$primary_key
      rn = self$backend$rownames
      pk_in_backend = TRUE
      type_check = TRUE

      if (is.data.frame(data)) {
        pk_in_backend = pk %chin% names(data)
        type_check = FALSE # done by auto-converter

        keep_cols = intersect(names(data), self$col_info$id)
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
        error_input("Cannot rbind data to task '%s', duplicated row ids", self$id)
      }

      # columns with these roles must be present in data
      mandatory_roles = c("target", "feature", "group", "stratum", "order", "offset", "weights_learner", "weights_measure")
      mandatory_cols = unlist(private$.col_roles[mandatory_roles], use.names = FALSE)
      missing_cols = setdiff(mandatory_cols, data$colnames)
      if (length(missing_cols)) {
        error_input("Cannot rbind data to task '%s', missing the following mandatory columns: %s", self$id, str_collapse(missing_cols))
      }

      # merge col infos
      tab = merge(self$col_info, col_info(data), by = "id",
        all.x = TRUE, all.y = FALSE, suffixes = c("", "_y"), sort = TRUE)

      # type check
      if (type_check) {
        type = type_y = NULL
        ii = head(tab[type != type_y, which = TRUE], 1L)
        if (length(ii)) {
          error_input("Cannot rbind to task: Types do not match for column: %s (%s != %s)", tab$id[ii], tab$type[ii], tab$type_y[ii])
        }
      }

      # merge factor levels
      ii = tab[type %chin% c("factor", "ordered"), which = TRUE]
      for (i in ii) {
        x = tab[["levels"]][[i]]
        y = tab[["levels_y"]][[i]]
        if (any(y %nin% x)) {
          set(tab, i = i, j = "levels", value = list(union(x, y)))
          set(tab, i = i, j = "fix_factor_levels", value = TRUE)
        }
      }
      tab[, c("type_y", "levels_y") := list(NULL, NULL)]

      # everything looks good, modify task
      private$.hash = NULL
      self$backend = DataBackendRbind$new(self$backend, data)
      self$col_info = tab[]
      private$.row_roles$use = c(private$.row_roles$use, data$rownames)

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
    #' @examples
    #' task = tsk("penguins")
    #' task$cbind(data.table(extra_col = seq_len(task$nrow)))
    #' head(task$data(cols = "extra_col"))
    cbind = function(data) {
      assert_has_backend(self)
      pk = self$backend$primary_key

      if (is.data.frame(data)) {
        # binding data with 0 rows is explicitly allowed
        if (ncol(data) == 0L) {
          return(invisible(self))
        }

        row_ids = if (pk %nin% names(data)) {
          data[[pk]] = self$row_ids
        }

        data = as_data_backend(data, primary_key = pk)
      } else {
        assert_backend(data)
        if (data$ncol <= 1L) {
          return(invisible(self))
        }
        assert_set_equal(self$row_ids, data$rownames)
      }

      # update col_info for existing columns
      ci = col_info(data)
      self$col_info = ujoin(self$col_info, ci, key = "id")

      # add rows to col_info for new columns
      self$col_info = rbindlist(list(
        self$col_info,
        insert_named(ci[!list(self$col_info), on = "id"], list(label = NA_character_, fix_factor_levels = FALSE))
      ), use.names = TRUE)
      setkeyv(self$col_info, "id")

      # add new features
      private$.hash = NULL
      private$.col_hashes = NULL
      col_roles = private$.col_roles
      private$.col_roles$feature = union(col_roles$feature, setdiff(data$colnames, c(pk, col_roles$target)))

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
    #' @examples
    #' task = tsk("penguins")
    #' task$rename("body_mass", "mass")
    #' task$feature_names
    rename = function(old, new) {
      assert_has_backend(self)
      private$.hash = NULL
      private$.col_hashes = NULL
      self$backend = DataBackendRename$new(self$backend, old, new)
      setkeyv(self$col_info[old, ("id") := new, on = "id"], "id")
      private$.col_roles = map(private$.col_roles, map_values, old = old, new = new)
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
    #' Duplicated row ids are explicitly allowed, so you can add replicate an observation by repeating its
    #' `row_id`.
    #'
    #' @return
    #' Returns the object itself, but modified **by reference**.
    #' You need to explicitly `$clone()` the object beforehand if you want to keeps
    #' the object in its previous state.
    #' @examples
    #' task = tsk("penguins")
    #' task$set_row_roles(1:5, remove_from = "use")
    set_row_roles = function(rows, roles = NULL, add_to = NULL, remove_from = NULL) {
      assert_has_backend(self)
      assert_subset(rows, self$backend$rownames)

      private$.row_hash = NULL
      private$.hash = NULL
      private$.row_roles = task_set_roles(private$.row_roles, rows, roles, add_to, remove_from, allow_duplicated = TRUE)

      invisible(self)
    },

    #' @description
    #' Modifies the roles in `$col_roles` **in-place**.
    #' See `$col_roles` for a list of possible roles.
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
    #' Roles are first set exclusively (argument `roles`), then added (argument `add_to`) and finally removed (argument `remove_from`) from different roles.
    #' Duplicated columns are removed from the same role.
    #' For tasks that only allow one target, the target column cannot be set with `$set_col_roles()`.
    #' Use the `$col_roles` field to swap the target column.
    #'
    #' @return
    #' Returns the object itself, but modified **by reference**.
    #' You need to explicitly `$clone()` the object beforehand if you want to keeps
    #' the object in its previous state.
    #' @examples
    #' task = tsk("penguins")
    #' task$set_col_roles("sex", roles = "stratum")
    #' task$col_roles$stratum
    set_col_roles = function(cols, roles = NULL, add_to = NULL, remove_from = NULL) {
      assert_has_backend(self)
      assert_subset(cols, self$col_info$id)

      private$.hash = NULL
      private$.col_hashes = NULL
      new_roles = task_set_roles(private$.col_roles, cols, roles, add_to, remove_from)
      private$.col_roles = task_check_col_roles(self, new_roles)

      invisible(self)
    },

    #' @description
    #' Set levels for columns of type `factor` and `ordered` in field `col_info`.
    #' You can add, remove or reorder the levels, affecting the data returned by
    #' `$data()` and `$levels()`.
    #' If you just want to remove unused levels, use `$droplevels()` instead.
    #'
    #' Note that factor levels which are present in the data but not listed in the task as
    #' valid levels are converted to missing values.
    #'
    #' @param levels (named `list()` of `character()`)\cr
    #'   List of character vectors of new levels, named by column names.
    #'
    #' @return Modified `self`.
    #' @examples
    #' task = tsk("penguins")
    #' task$set_levels(list(sex = c("male", "female", "unknown")))
    #' task$levels("sex")
    set_levels = function(levels) {
      assert_list(levels, types = "character", names = "unique", any.missing = FALSE)
      assert_subset(names(levels), self$col_info$id)

      tab = enframe(lapply(levels, unname), name = "id", value = "levels")
      tab$fix_factor_levels = TRUE

      private$.hash = NULL
      self$col_info = ujoin(self$col_info, tab, key = "id")

      invisible(self)
    },


    #' @description
    #' Updates the cache of stored factor levels, removing all levels not present in the current set of active rows.
    #' `cols` defaults to all columns with storage type "factor" or "ordered".
    #' @return Modified `self`.
    #' @examples
    #' task = tsk("penguins")
    #' task$set_levels(list(sex = c("male", "female", "unknown")))
    #' task$levels("sex")
    droplevels = function(cols = NULL) {
      assert_has_backend(self)
      tab = self$col_info[get("type") %chin% c("factor", "ordered"), c("id", "levels", "fix_factor_levels"), with = FALSE]
      if (!is.null(cols)) {
        tab = tab[list(cols), on = "id", nomatch = NULL]
      }

      # update levels
      # note that we assume that new_levels is a subset of levels!
      new_levels = NULL
      tab$new_levels = self$backend$distinct(rows = self$row_ids, cols = tab$id)
      tab = tab[lengths(levels) > lengths(new_levels)]
      tab[, c("levels", "fix_factor_levels") := list(Map(intersect, levels, new_levels), TRUE)]

      private$.hash = NULL
      self$col_info = ujoin(self$col_info, remove_named(tab, "new_levels"), key = "id")

      invisible(self)
    },


    #' @description
    #' Cuts numeric variables into new factors columns which are added to the task with role
    #' `"stratum"`.
    #' This ensures that all training and test splits contain observations from all bins.
    #' The columns are named `"..stratum_[col_name]"`.
    #'
    #' @param cols (`character()`)\cr
    #'   Names of columns to operate on.
    #' @param bins (`integer()`)\cr
    #'   Number of bins to cut into (passed to [cut()] as `breaks`).
    #'   Replicated to have the same length as `cols`.
    #' @return self (invisibly).
    #' @examples
    #' task = tsk("penguins")
    #' task$add_strata("flipper_length", bins = 4)
    add_strata = function(cols, bins = 3L) {
      assert_names(cols, "unique", subset.of = self$backend$colnames)
      bins = assert_integerish(bins, any.missing = FALSE, coerce = TRUE)

      col_types = fget_keys(self$col_info, i = cols, j = "type", key = "id")
      ii = wf(col_types %nin% c("integer", "numeric"))
      if (length(ii)) {
        error_input("For `add_strata`, all columns must be numeric, but '%s' is not", cols[ii])
      }

      strata = pmap_dtc(list(self$data(cols = cols), bins), cut, include.lowest = TRUE)
      setnames(strata, sprintf("..stratum_%s", cols))
      self$cbind(strata)
      self$set_col_roles(names(strata), roles = "stratum")
    },

    #' @description
    #' Certain operations change the view on the data, e.g., `$filter()` or `$select()`.
    #' This operation queries the [DataBackend] for all data required in the active view and
    #' replaces the internal [DataBackend] with the new one. In some scenarios this helps to
    #' free up memory or speeds up accesses to the data, especially after several `$rbind()`
    #' and `$cbind()` operations.
    #'
    #' @details
    #' For tasks containing the same observation more than once (duplicates in `$row_ids`),
    #' the resulting backend contains it only once.
    #'
    #' @param internal_valid_task (`logical(1)`)\cr
    #'   Also materialize the internal validation task. Default is `TRUE`.
    #'
    #' @return self (invisibly).
    #' @examples
    #' task = tsk("iris")
    #' task$backend$nrow
    #' task$filter(1:120)
    #' task$backend$nrow
    materialize_view = function(internal_valid_task = TRUE) {
      assert_flag(internal_valid_task)

      b = self$backend
      ..cns = union(b$primary_key, unlist(private$.col_roles, use.names = FALSE))
      dt = b$data(rows = unique(self$row_ids), cols = ..cns)
      self$backend = as_data_backend(dt, primary_key = b$primary_key)
      self$col_info = setkeyv(self$col_info[list(..cns), on = "id"], "id")

      if (internal_valid_task && !is.null(private$.internal_valid_task)) {
        private$.internal_valid_task$materialize_view(FALSE)
      }
      invisible(self)
    }
  ),

  active = list(
    #' @template field_id
    id = function(rhs) {
      if (missing(rhs)) {
        return(private$.id)
      }

      private$.hash = NULL
      private$.id = assert_string(rhs, min.chars = 1L)
    },

    #' @field internal_valid_task (`Task` or `integer()` or `NULL`)\cr
    #' Optional validation task that can, e.g., be used for early stopping with learners such as XGBoost.
    #' See also the `$validate` field of [`Learner`].
    #' If integers are assigned they are removed from the primary task and an internal validation task
    #' with those ids is created from the primary task using only those ids.
    #' When assigning a new task, it is always cloned.
    internal_valid_task = function(rhs) {
      if (missing(rhs)) {
        return(invisible(private$.internal_valid_task))
      }
      private$.hash = NULL
      if (is.null(rhs)) {
        private$.internal_valid_task = NULL
        return(invisible(private$.internal_valid_task))
      }
      private$.hash = NULL

      if (test_integerish(rhs)) {
        train_ids = setdiff(self$row_ids, rhs)
        rhs = self$clone(deep = TRUE)$filter(rhs)
        rhs$internal_valid_task = NULL
        self$row_roles$use = train_ids
      } else {
        if (!is.null(rhs$internal_valid_task)) { # avoid recursive structures
          error_input("Trying to assign task '%s' as a validation task, remove its validation task first.", rhs$id)
        }
        assert_task(rhs, task_type = self$task_type)
        rhs = rhs$clone(deep = TRUE)
      }

      ci1 = self$col_info
      ci2 = rhs$col_info
      # don't do this too strictly, some column roles might just be important during training (weights)
      cols = unlist(self$col_roles[c("target", "feature")], use.names = FALSE)
      walk(cols, function(.col) {
        if (.col %nin% ci2$id) {
          error_input("Primary task has column '%s' which is not present in the validation task.", .col)
        }
        if (ci1[get("id") == .col, "type"]$type != ci2[get("id") == .col, "type"]$type) {
          error_input("The type of column '%s' from the validation task differs from the type in the primary task.", .col)
        }
      })

      private$.internal_valid_task = rhs
      if (private$.internal_valid_task$nrow == 0) {
        warning_input("Internal validation task has 0 observations.")
      }
      invisible(private$.internal_valid_task)
    },

    #' @field hash (`character(1)`)\cr
    #' Hash (unique identifier) for this object.
    #' The hash is calculated based on the complete task object and `$row_ids`.
    #' If an internal validation task is set, the hash is recalculated.
    hash = function(rhs) {
      if (is.null(private$.hash)) {
        private$.hash = task_hash(self, self$row_ids, ignore_internal_valid_task = FALSE)
      }

      private$.hash
    },

    #' @field row_hash (`character(1)`)\cr
    #' Hash (unique identifier) calculated based on the row ids.
    row_hash = function(rhs) {
      assert_ro_binding(rhs)
      if (is.null(private$.row_hash)) {
        private$.row_hash = calculate_hash(self$row_ids)
      }
      private$.row_hash
    },

    #' @field row_ids (positive `integer()`)\cr
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
      nn = private$.col_roles$name
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
    #' * `"weights_learner"`: If the task has observation weights with this role, they are passed to the [Learner] during train.
    #'    The use of weights can be disabled via by setting the learner's hyperparameter `use_weights` to `FALSE`.
    #' * `"weights_measure"`: If the task has observation weights with this role, they are passed to the [Measure] for weighted scoring.
    #'    The use of weights can be disabled via by setting the measure's hyperparameter `use_weights` to `FALSE`.
    #' * `"offset"`: The task includes one or more offset columns specifying fixed adjustments for model training and possibly for prediction (role `"offset"`).
    #' * `"ordered"`: The task has columns which define the row order (role `"order"`).
    #'
    #' Note that above listed properties are calculated from the `$col_roles`, and may not be set explicitly.
    properties = function(rhs) {
      assert_ro_binding(rhs)
      prop_roles = c(
        groups = "group",
        strata = "stratum",
        weights_learner = "weights_learner",
        weights_measure = "weights_measure",
        offset = "offset",
        ordered = "order"
      )
      c(private$.properties, names(prop_roles)[lengths(private$.col_roles[prop_roles]) > 0L])
    },

    #' @field row_roles (named `list()`)\cr
    #' Each row (observation) can have an arbitrary number of roles in the learning task:
    #'
    #' - `"use"`: Use in train / predict / resampling.
    #'
    #' `row_roles` is a named list whose elements are named by row role and each element is an `integer()` vector of row ids.
    #' To alter the roles, just modify the list, e.g. with  \R's set functions ([intersect()], [setdiff()], [union()], \ldots).
    row_roles = function(rhs) {
      if (missing(rhs)) {
        return(private$.row_roles)
      }

      assert_has_backend(self)
      assert_list(rhs, .var.name = "row_roles")
      if ("test" %chin% names(rhs) || "holdout" %chin% names(rhs)) {
        error_input("Setting row roles 'test'/'holdout' is no longer possible.")
      }
      assert_names(names(rhs), "unique", permutation.of = mlr_reflections$task_row_roles, .var.name = "names of row_roles")
      rhs = map(rhs, assert_row_ids, .var.name = "elements of row_roles")
      private$.row_hash = NULL
      private$.hash = NULL
      private$.row_roles = rhs
    },

    #' @field col_roles (named `list()`)\cr
    #' Each column can be in one or more of the following groups to fulfill different roles:
    #'
    #' * `"feature"`: Regular feature used in the model fitting process.
    #' * `"target"`: Target variable. Most tasks only accept a single target column.
    #' * `"name"`: Row names / observation labels. To be used in plots. Can be queried with `$row_names`.
    #'   Not more than a single column can be associated with this role.
    #' * `"order"`: Data returned by `$data()` is ordered by this column (or these columns).
    #'   Columns must be sortable with [order()].
    #' * `"group"`: During resampling, observations with the same value of the variable with role "group" are marked as "belonging together".
    #'   For each resampling iteration, observations of the same group will be exclusively assigned to be either in the training set or in the test set.
    #'   Not more than a single column can be associated with this role.
    #' * `"stratum"`: Stratification variables. Multiple discrete columns may have this role.
    #' * `"weights_learner"`: If the task has observation weights with this role, they are passed to the [Learner] during train.
    #'    The use of weights can be disabled via by setting the learner's hyperparameter `use_weights` to `FALSE`.
    #' * `"weights_measure"`: If the task has observation weights with this role, they are passed to the [Measure] for weighted scoring.
    #'    The use of weights can be disabled via by setting the measure's hyperparameter `use_weights` to `FALSE`.
    #' * `"offset"`: Numeric columns used to specify fixed adjustments for model training.
    #'   Some models use offsets to simply shift predictions, while others incorporate them to boost predictions from a baseline model.
    #'   For learners supporting offsets in multiclass settings, an offset column must be provided for each target class.
    #'   These columns must follow the naming convention `"offset_{target_class_name}"`.
    #'   For an example of a learner that supports offsets, see `LearnerClassifXgboost`  of \CRANpkg{mlr3learners}.
    #'
    #' `col_roles` is a named list whose elements are named by column role and each element is a `character()` vector of column names.
    #' To alter the roles, just modify the list, e.g. with \R's set functions ([intersect()], [setdiff()], [union()], \ldots).
    #' The method `$set_col_roles` provides a convenient alternative to assign columns to roles.
    #'
    #' The roles `weights_learner` and `weights_measure` may only point to a single numeric column, but they can
    #' all point to the same column or different columns. Weights must be non-negative numerics with at least one weight being > 0.
    #' They don't necessarily need to sum up to 1.
    col_roles = function(rhs) {
      if (missing(rhs)) {
        return(private$.col_roles)
      }

      assert_has_backend(self)
      qassertr(rhs, "S[1,]", .var.name = "col_roles")
      assert_names(names(rhs), "unique", permutation.of = mlr_reflections$task_col_roles[[self$task_type]])
      assert_subset(unlist(rhs, use.names = FALSE), setdiff(self$col_info$id, self$backend$primary_key), .var.name = "elements of col_roles")

      private$.hash = NULL
      private$.col_hashes = NULL
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

    #' @field n_features (`integer(1)`)\cr
    #' Returns the total number of columns with role "feature" (i.e. the number of "active" features in the task).
    n_features = function(rhs) {
      assert_ro_binding(rhs)
      length(private$.col_roles$feature)
    },

    #' @field feature_types ([data.table::data.table()])\cr
    #' Returns a table with columns `id` and `type` where `id` are the column names of "active"
    #' features of the task and `type` is the storage type.
    feature_types = function(rhs) {
      assert_ro_binding(rhs)
      setkeyv(self$col_info[list(private$.col_roles$feature), c("id", "type"), on = "id"], "id")
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
    #' Deprecated, use `$weights_learner` instead.
    weights = function(rhs) {
      assert_ro_binding(rhs)
      .Deprecated("Task$weights_learner", old = "Task$weights")
      self$weights_learner
    },

    #' @field weights_learner ([data.table::data.table()])\cr
    #' Returns the observation weights used for training a [Learner] (column role `weights_learner`)
    #' as a `data.table` with the following columns:
    #'
    #' * `row_id` (`integer()`), and
    #' * `weight` (`numeric()`).
    #'
    #' Returns `NULL` if there are is no column with the designated role.
    weights_learner = function(rhs) {
      assert_has_backend(self)
      assert_ro_binding(rhs)
      weight_cols = private$.col_roles[["weights_learner"]]
      if (length(weight_cols) == 0L) {
        return(NULL)
      }
      data = self$backend$data(private$.row_roles$use, c(self$backend$primary_key, weight_cols))
      setnames(data, c("row_id", "weight"))[]
    },

    #' @field weights_measure ([data.table::data.table()])\cr
    #' Returns the observation weights used for scoring a prediction with a [Measure] (column role `weights_measure`)
    #' as a `data.table` with the following columns:
    #'
    #' * `row_id` (`integer()`), and
    #' * `weight` (`numeric()`).
    #'
    #' Returns `NULL` if there are is no column with the designated role.
    weights_measure = function(rhs) {
      assert_has_backend(self)
      assert_ro_binding(rhs)
      weight_cols = private$.col_roles[["weights_measure"]]
      if (length(weight_cols) == 0L) {
        return(NULL)
      }
      data = self$backend$data(private$.row_roles$use, c(self$backend$primary_key, weight_cols))
      setnames(data, c("row_id", "weight"))[]
    },
    #' @field offset ([data.table::data.table()])\cr
    #' If the task has a column with designated role `"offset"`, a table with two or more columns:
    #'
    #' * `row_id` (`integer()`), and
    #' * offset variable(s) (`numeric()`).
    #'
    #' For regression or binary classification tasks, there will be only a single-column offset.
    #' For multiclass tasks, it may return multiple offset columns, one for each target class.
    #' If there is only one offset column, it will be named as `offset`.
    #'
    #' If there are no columns with the `"offset"` role, `NULL` is returned.
    offset = function(rhs) {
      assert_has_backend(self)
      assert_ro_binding(rhs)
      offset_cols = private$.col_roles$offset
      if (length(offset_cols) == 0L) {
        return(NULL)
      }

      data = self$backend$data(private$.row_roles$use, c(self$backend$primary_key, offset_cols))
      if (length(offset_cols) == 1L) {
        setnames(data, c("row_id", "offset"))[]
      } else  {
        setnames(data, c("row_id", offset_cols))[]
      }
    },

    #' @field labels (named `character()`)\cr
    #'   Retrieve `labels` (prettier formated names) from columns.
    #'   Internally queries the column `label` of the table in field `col_info`.
    #'   Columns ids referenced by the name of the vector, the labels are the actual string values.
    #'
    #'   Assigning to this column update the task by reference.
    #'   You have to provide a character vector of labels, named with column ids.
    #'   To remove a label, set it to `NA`.
    #'   Alternatively, you can provide a [data.frame()] with the two columns
    #'   `"id"` and `"label"`.
    labels = function(rhs) {
      active = union(self$target_names, self$feature_names)

      if (missing(rhs)) {
        tab = ijoin(self$col_info, active, c("id", "label"), "id")
        return(set_names(tab[["label"]], tab[["id"]]))
      }

      if (is.data.frame(rhs)) { # convert to named character
        assert_data_frame(rhs, ncols = 2L)
        assert_names(names(rhs), permutation.of = c("id", "label"))
        rhs = set_names(rhs[["label"]], rhs[["id"]])
      }

      assert_names(names(rhs), type = "unique")
      assert_subset(names(rhs), active)
      self$col_info[list(names(rhs)), "label" := rhs, on = "id"]

      invisible(self)
    },

    #' @template field_col_hashes
    col_hashes = function() {
      if (is.null(private$.col_hashes)) {
        private$.col_hashes = self$backend$col_hashes[setdiff(unlist(private$.col_roles, use.names = FALSE), self$backend$primary_key)]
      }
      private$.col_hashes
    },

    #' @field characteristics (`list()`)\cr
    #' List of characteristics of the task, e.g. `list(n = 5, p = 7)`.
    characteristics = function(rhs) {
      if (missing(rhs)) {
        return(private$.characteristics)
      }

      private$.characteristics = assert_list(rhs, null.ok = TRUE)
      private$.hash = NULL
    },

    #' @field row_ids_backend (`integer()`)\cr
    #' Returns all row ids from the backend, regardless of their roles.
    #' This is different from `$row_ids` which only returns rows with role "use".
    row_ids_backend = function(rhs) {
      assert_ro_binding(rhs)
      self$backend$rownames
    }
  ),

  private = list(
    .internal_valid_task = NULL,
    .id = NULL,
    .properties = NULL,
    .col_roles = NULL,
    .row_roles = NULL,
    .hash = NULL,
    .col_hashes = NULL,
    .characteristics = NULL,
    .row_hash = NULL,

    deep_clone = function(name, value) {
      # NB: DataBackends are never copied!
      if (name == "col_info") {
        copy(value)
      } else if (name == ".internal_valid_task" && !is.null(value)) {
        value$clone(deep = TRUE)
      } else {
        value
      }
    }
  )
)

task_set_roles = function(li, elements, roles = NULL, add_to = NULL, remove_from = NULL, allow_duplicated = FALSE) {
  if (!is.null(roles)) {
    assert_subset(roles, names(li))
    if (allow_duplicated) {
      for (role in roles) {
        li[[role]] = c(li[[role]], elements)
      }

      for (role in setdiff(names(li), roles)) {
        li[[role]] = li[[role]][li[[role]] %nin% elements]
      }
    } else {
      for (role in roles) {
        li[[role]] = union(li[[role]], elements)
      }

      for (role in setdiff(names(li), roles)) {
        li[[role]] = setdiff(li[[role]], elements)
      }
    }
  }

  if (!is.null(add_to)) {
    assert_subset(add_to, names(li))
    if (allow_duplicated) {
      for (role in add_to) {
        li[[role]] = c(li[[role]], elements)
      }
    } else {
      for (role in add_to) {
        li[[role]] = union(li[[role]], elements)
      }

    }
  }

  if (!is.null(remove_from)) {
    assert_subset(remove_from, names(li))
    if (allow_duplicated) {
      for (role in remove_from) {
        li[[role]] = li[[role]][li[[role]] %nin% elements]
      }
    } else {
      for (role in remove_from) {
        li[[role]] = setdiff(li[[role]], elements)
      }
    }
  }

  li
}

#' @title Check Column Roles
#'
#' @description
#' Internal function to check column roles.
#'
#' @param task ([Task])\cr
#'  Task.
#' @param new_roles (`list()`)\cr
#'  Column roles.
#'
#' @keywords internal
#' @export
task_check_col_roles = function(task, new_roles, ...) {
  UseMethod("task_check_col_roles")
}

#' @rdname task_check_col_roles
#' @export
task_check_col_roles.Task = function(task, new_roles, ...) {
  if ("weight" %in% names(new_roles)) {
    error_input("Task role 'weight' is deprecated, use 'weights_learner' instead")
  }

  for (role in c("group", "name", "weights_learner", "weights_measure")) {
    if (length(new_roles[[role]]) > 1L) {
      error_input("There may only be up to one column with role '%s'", role)
    }
  }

  # check weights
  for (role in c("weights_learner", "weights_measure")) {
    if (length(new_roles[[role]]) > 0L) {
      col = task$backend$data(seq(task$backend$nrow), cols = new_roles[[role]])
      assert_numeric(col[[1L]], lower = 0, any.missing = FALSE, .var.name = names(col))
    }
  }

  # check name
  if (length(new_roles[["name"]])) {
    row_names = task$backend$data(task$backend$rownames, cols = new_roles[["name"]])
    if (!is.character(row_names[[1L]]) && !is.factor(row_names[[1L]])) {
      error_input("Assertion on '%s' failed: Must be of type 'character' or 'factor', not %s", names(row_names), class(row_names[[1]]))
    }
  }

  # check offset
  if (length(new_roles[["offset"]]) && any(fget_keys(task$col_info, new_roles[["offset"]], "type", key = "id") %nin% c("numeric", "integer"))) {
    error_input("Offset column(s) %s must be a numeric or integer column", paste0("'", new_roles[["offset"]], "'", collapse = ","))
  }

  if (length(new_roles[["offset"]]) && any(task$missings(cols = new_roles[["offset"]]) > 0)) {
    missings = task$missings(cols = new_roles[["offset"]])
    missings = names(missings[missings > 0])
    error_input("Offset column(s) %s contain missing values", paste0("'", missings, "'", collapse = ","))
  }

  new_roles
}

#' @rdname task_check_col_roles
#' @export
task_check_col_roles.TaskClassif = function(task, new_roles, ...) {

  # check target
  if (length(new_roles[["target"]]) > 1L) {
    error_input("There may only be up to one column with role 'target'")
  }

  if (length(new_roles[["target"]]) && any(fget_keys(task$col_info, new_roles[["target"]], "type", key = "id") %nin% c("factor", "ordered"))) {
    error_input("Target column(s) %s must be a factor or ordered factor", paste0("'", new_roles[["target"]], "'", collapse = ","))
  }

  if (length(new_roles[["offset"]]) > 1L && length(task$class_names) == 2L) {
    error_input("There may only be up to one column with role 'offset' for binary classification tasks")
  }

  if (length(new_roles[["offset"]]) > 1L) {
    expected_names = paste0("offset_", task$class_names)
    expect_subset(new_roles[["offset"]], expected_names, label = "col_roles")
  }

  NextMethod()
}

#' @rdname task_check_col_roles
#' @export
task_check_col_roles.TaskRegr = function(task, new_roles, ...) {
  for (role in c("target", "offset")) {
    if (length(new_roles[[role]]) > 1L) {
      error_input("There may only be up to one column with role '%s'", role)
    }
  }

  if (length(new_roles[["target"]]) && any(fget_keys(task$col_info, new_roles[["target"]], "type", key = "id") %nin% c("numeric", "integer"))) {
    error_input("Target column '%s' must be a numeric or integer column", paste0("'", new_roles[["target"]], "'", collapse = ","))
  }

  NextMethod()
}

#' @rdname task_check_col_roles
#' @export
task_check_col_roles.TaskSupervised = function(task, new_roles, ...) {

  # check target
  if (length(new_roles$target) == 0L) {
    error_input("Supervised tasks need at least one target column")
  }

  NextMethod()
}

#' @rdname task_check_col_roles
#' @export
task_check_col_roles.TaskUnsupervised = function(task, new_roles, ...) {

  # check target
  if (length(new_roles$target) != 0L) {
    error_input("Unsupervised tasks may not have a target column")
  }

  NextMethod()
}

#' @title Column Information for Backend
#'
#' @description
#' Collects column information for backend.
#'
#' Currently, this includes:
#' * storage type
#' * levels (factor / ordered), but not for the primary key column
#'
#' @param x (any)\cr
#'   A backend-like object for which to retrieve column information.
#' @param ... (any)\cr
#'   Additional arguments.
#'
#' @export
#' @keywords internal
col_info = function(x, ...) {
  UseMethod("col_info")
}

#' @rdname col_info
#' @param primary_key (`character()`)\cr
#'   The primary key of the backend.
#' @export
col_info.data.table = function(x, primary_key = character(), ...) { # nolint
  types = map_chr(x, function(x) class(x)[1L])
  discrete = setdiff(names(types)[types %chin% c("factor", "ordered")], primary_key)
  levels = insert_named(named_list(names(types)), lapply(x[, discrete, with = FALSE], distinct_values, drop = FALSE))
  data.table(id = names(types), type = unname(types), levels = levels, key = "id")
}

#' @rdname col_info
#' @export
col_info.DataBackend = function(x, ...) { # nolint
  types = map_chr(x$head(1L), function(x) class(x)[1L])
  discrete = setdiff(names(types)[types %chin% c("factor", "ordered")], x$primary_key)
  levels = insert_named(named_list(names(types)), x$distinct(rows = NULL, cols = discrete))
  data.table(id = names(types), type = unname(types), levels = levels, key = "id")
}

#' @export
as.data.table.Task = function(x, ...) { # nolint
  x$data()
}

#' @export
head.Task = function(x, n = 6L, ...) { # nolint
  assert_number(n, na.ok = FALSE)
  x$data(rows = head(x$row_ids, n))
}

#' @export
tail.Task = function(x, n = 6L, ...) { # nolint
  assert_number(n, na.ok = FALSE)
  x$data(rows = tail(x$row_ids, n))
}

task_rm_backend = function(task) {
  # fix task hash
  ee = get_private(task)
  ee$.hash = force(task$hash)
  ee$.col_hashes = force(task$col_hashes)
  ee$.internal_valid_task$backend = NULL

  # NULL backend
  task$backend = NULL

  task
}


#' @export
rd_info.Task = function(obj, section, ...) { # nolint
  x = c("",
    sprintf("* Task type: %s", rd_format_string(obj$task_type)),
    sprintf("* Dimensions: %ix%i", obj$nrow, obj$ncol),
    sprintf("* Properties: %s", rd_format_string(obj$properties)),
    sprintf("* Has Missings: `%s`", any(obj$missings() > 0L)),
    sprintf("* Target: %s", rd_format_string(obj$target_names)),
    sprintf("* Features: %s", rd_format_string(obj$feature_names))
  )
  paste(x, collapse = "\n")
}

#' @export
summary.Task = function(object, limit = object$nrow, ...) {
  summary(head(object, limit))
}

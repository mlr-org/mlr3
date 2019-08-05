#' @include mlr_reflections.R

task_set_row_role = function(self, rows, new_roles, exclusive = TRUE) {
  rows = assert_row_ids(rows, type = typeof(self$row_roles$use))
  assert_subset(rows, self$backend$rownames)
  assert_subset(new_roles, mlr_reflections$task_row_roles)
  assert_flag(exclusive)

  for (role in new_roles) {
    self$row_roles[[role]] = union(self$row_roles[[role]], rows)
  }

  if (exclusive) {
    for (role in setdiff(names(self$row_roles), new_roles)) {
      self$row_roles[[role]] = setdiff(self$row_roles[[role]], rows)
    }
  }
}

task_set_col_role = function(self, cols, new_roles, exclusive = TRUE) {

  assert_character(cols, any.missing = FALSE)
  assert_subset(cols, self$col_info$id)
  assert_subset(new_roles, mlr_reflections$task_col_roles[[self$task_type]])
  assert_flag(exclusive)

  for (role in new_roles) {
    self$col_roles[[role]] = union(self$col_roles[[role]], cols)
  }

  if (exclusive) {
    for (role in setdiff(names(self$col_roles), new_roles)) {
      self$col_roles[[role]] = setdiff(self$col_roles[[role]], cols)
    }
  }

  # update weights and groups property
  for (role in c("weights", "groups")) {
    n = length(self$col_roles[[role]])
    if (n == 0L) {
      self$properties = setdiff(self$properties, role)
    } else if (n == 1L) {
      self$properties = union(self$properties, role)
    } else {
      stopf("Multiple columns with role '%s' not supported", role)
    }
  }
}

check_new_row_ids = function(task, data, type) {
  pk = task$backend$primary_key
  row_ids = data[[pk]]
  task_row_ids = task$row_roles$use

  assert_atomic_vector(row_ids, any.missing = FALSE, unique = TRUE)
  if (typeof(task_row_ids) != typeof(row_ids)) {
    stopf("Cannot mutate task: Key column '%s' has wrong type", pk)
  }

  switch(type,
    "disjunct" = {
      if (any(row_ids %in% task_row_ids)) {
        stopf("Cannot mutate task: Duplicated row_ids")
      }
    },
    "setequal" = {
      if (!setequal(row_ids, task_row_ids)) {
        stopf("Cannot mutate task: row_ids do not match")
      }
    },
    "subset" = {
      if (!all(row_ids %in% task_row_ids)) {
        stopf("Cannot mutate task: Extra row ids")
      }
  })
}

convert_matching_types = function(col_info, data) {
  pmap(col_info, function(id, type, levels) {
    cur_col = data[[id]]
    cur_type = class(cur_col)[1L]

    if (type != cur_type && any(c(type, cur_type) %nin% c("character", "factor", "ordered"))) {
      if (allMissing(cur_col)) {
        if (type %in% c("factor", "ordered")) {
          cur_col = as_factor(cur_col, levels = levels, ordered = (type == "ordered"))
        } else {
          storage.mode(cur_col) = type
        }
        data[, (id) := cur_col]
      } else {
        stopf("Cannot rbind task: Types do not match for column: %s (%s != %s)", id, type, cur_type)
      }
    }
  })
}

# Performs the following steps to virtually rbind data to the task:
# 1. Check that an rbind is feasible
# 2. Overwrite self$backend with new backend
# 3. Update row_roles
# 4. Update col_info
task_rbind = function(self, data) {
  if (all(dim(data) == 0L)) {
    return(invisible(self))
  }

  # 1. Check that an rbind is feasible
  assert_data_frame(data, min.cols = 1L)
  data = as.data.table(data)
  pk = self$backend$primary_key

  ## 1.1 Check for primary key column and auto-increment
  if (pk %in% names(data)) {
    check_new_row_ids(self, data, "disjunct")
  } else {
    rids = self$row_ids
    if (is.integer(rids)) {
      data[[pk]] = max(rids) + seq_row(data)
    } else {
      data[[pk]] = sprintf("%s_%i", basename(tempfile("rbind_")), seq_row(data))
    }
  }

  # nothing to rbind
  if (ncol(data) == 1L) {
    return(invisible(self))
  }

  ## 1.2 Check for set equality of column names
  assert_set_equal(names(data), c(unlist(self$col_roles, use.names = FALSE), pk))

  ## 1.4 Check that types are matching
  col_info = self$col_info[unique(unlist(self$col_roles, use.names = FALSE)), on = "id"]
  convert_matching_types(col_info, data)
  ci = col_info(data, primary_key = pk)

  # 2. Overwrite self$backend with new backend
  rows_self = unlist(self$row_roles, use.names = FALSE)
  self$backend = DataBackendRbind$new(self$backend, DataBackendDataTable$new(data, pk), rows_self, data[[pk]])

  # 3. Update row_roles
  self$row_roles$use = c(self$row_roles$use, data[[pk]])

  # 4. Update col_info
  vunion = function(x, y) Map(union, x, y)
  self$col_info = self$col_info[ci[, c("id", "levels"), with = FALSE], on = "id", nomatch = 0L]
  self$col_info[get("type") %in% c("factor", "ordered", "character"), "levels" := list(vunion(get("levels"), get("i.levels")))]
  self$col_info[, "i.levels" := NULL]

  invisible(self)
}

# Performs the following steps to virtually cbind data to the task:
# 1. Check that an cbind is feasible
# 2. Overwrite self$backend with new backend
# 3. Update col_info
task_cbind = function(self, data) {
  if (all(dim(data) == 0L)) {
    return(invisible(self))
  }

  # 1. Check that an cbind is feasible
  assert_data_frame(data, nrows = self$nrow)
  data = as.data.table(data)
  pk = self$backend$primary_key

  ## 1.1 Check primary key column
  if (pk %in% names(data)) {
    check_new_row_ids(self, data, "setequal")
  } else {
    data[[pk]] = self$row_ids
  }

  # nothing to cbind
  if (ncol(data) == 1L) {
    return(invisible(self))
  }

  # 2. Overwrite self$backend with new backend
  b2 = DataBackendDataTable$new(data, pk)
  self$backend = DataBackendCbind$new(self$backend, b2)

  # 3. Update col_info
  ci = col_info(data)
  self$col_info = ujoin(self$col_info, ci, key = "id")
  self$col_info = rbind(self$col_info, ci[!list(self$col_info$id), on = "id"])
  setkeyv(self$col_info, "id")
  self$col_roles$feature = union(self$col_roles$feature, setdiff(names(data), pk))

  invisible(self)
}

task_rename = function(self, old, new) {
  self$backend = DataBackendRename$new(self$backend, old, new)
  setkeyv(self$col_info[old, ("id") := new, on = "id"], "id")
  self$col_roles = map(self$col_roles, map_values, old = old, new = new)
  invisible(self)
}

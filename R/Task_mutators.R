check_new_row_ids = function(task, data, type) {
  pk = task$backend$primary_key
  row_ids = data[[pk]]
  task_row_ids = task$row_roles$use

  assert_atomic_vector(row_ids, any.missing = FALSE, unique = TRUE)
  if (typeof(task_row_ids) != typeof(row_ids))
    stopf("Cannot mutate task: Key column '%s' has wrong type", pk)

  switch(type,
    "disjunct" = {
      if (any(row_ids %in% task_row_ids))
        stopf("Cannot mutate task: Duplicated row_ids")
    },
    "setequal" = {
      if (!setequal(row_ids, task_row_ids))
        stopf("Cannot mutate task: row_ids do not match")
    },
    "subset" = {
      if (!all(row_ids %in% task_row_ids))
        stopf("Cannot mutate task: Extra row ids")
    }
  )
}

check_matching_types = function(col_info_x, col_info_y) {
  cmp = function(x, y) {
    # character -> factor conversion is handled by data.table
    x != y & !(x %in% c("character", "factor") & y %in% c("character", "factor"))
  }
  joined = merge(col_info_x, col_info_y, by = "id")
  joined = head(joined[cmp(get("type.x"), get("type.y"))], 1L)
  if (nrow(joined)) {
    stopf("Cannot rbind task: Types do not match for column: %s (%s != %s)", joined$id, joined$type.x, joined$type.y)
  }
}

# Performs the following steps to virtually rbind data to the task:
# 1. Check that an rbind is feasible
# 2. Overwrite self$backend with new backend
# 3. Update row_roles
# 4. Update col_info
task_rbind = function(self, data) {
  # 1. Check that an rbind is feasible
  assert_data_frame(data, min.rows = 1L, min.cols = 1L)
  data = as.data.table(data)
  pk = self$backend$primary_key

  ## 1.1 Check for primary key column and auto-increment
  if (pk %nin% names(data)) {
    rids = self$row_ids[[1L]]
    if (is.integer(rids)) {
      data[[pk]] = max(rids) + seq_row(data)
    } else {
      data[[pk]] = sprintf("%s_%i", basename(tempfile("rbind_")), seq_row(data))
    }
  } else {
    check_new_row_ids(self, data, "disjunct")
  }

  ## 1.2 Check for set equality of column names
  assert_set_equal(names(data), c(unlist(self$col_roles, use.names = FALSE), pk))

  ## 1.4 Check that types are matching
  data_col_info = col_info(data, primary_key = pk)
  check_matching_types(self$col_info, data_col_info)

  # 2. Overwrite self$backend with new backend
  rows_self = unlist(self$row_roles, use.names = FALSE)
  self$backend = DataBackendRbind$new(self$backend, DataBackendDataTable$new(data, pk), rows_self, data[[pk]])

  # 3. Update row_roles
  self$row_roles$use = c(self$row_roles$use, data[[pk]])

  # 4. Update col_info
  self$col_info$levels = Map(union, self$col_info$levels, data_col_info$levels)


  invisible(self)
}

# Performs the following steps to virtually cbind data to the task:
# 1. Check that an cbind is feasible
# 2. Overwrite self$backend with new backend
# 3. Update col_info
task_cbind = function(self, data) {
  # 1. Check that an cbind is feasible
  assert_data_frame(data, min.rows = 1L, min.cols = 1L)
  data = as.data.table(data)
  pk = self$backend$primary_key

  ## 1.1 Check primary key column
  check_new_row_ids(self, data, "setequal")

  # 2. Overwrite self$backend with new backend
  b2 = DataBackendDataTable$new(data, pk)
  cols_self = unlist(self$col_roles, use.names = FALSE)
  self$backend = DataBackendCbind$new(self$backend, b2, cols_self, colnames(data))

  # 3. Update col_info
  data_col_info = col_info(data)
  self$col_info = setkeyv(rbindlist(list(self$col_info, data_col_info[!list(pk)])), "id")
  if (anyDuplicated(self$col_info, by = "id"))
    stopf("Duplicated columns")
  self$col_roles$feature = union(self$col_roles$feature, setdiff(names(data), pk))

  invisible(self)
}

task_replace_data = function(self, data, ...) {
  assert_data_frame(data, min.rows = 1L, min.cols = 1L)
  data = as.data.table(data)

  cols = remove_named(self$col_roles, c("target", "feature"))
  cols = unique(unlist(cols, use.names = FALSE))
  if (!all(cols %in% names(data)))
    stopf("Task contains columns with a special role (%s) which are not present in new data", str_collapse(cols))

  self$initialize(id = self$id, backend = as_data_backend(data), ...)
  self
}

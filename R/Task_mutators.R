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
# 2. Update row_roles
# 3. Update col_info
# 4. Overwrite self$backend with new backend
task_rbind = function(self, data) {
  # 1. Check that an rbind is feasible
  assert_data_frame(data, min.rows = 1L, min.cols = 1L)
  data = as.data.table(data)
  pk = self$backend$primary_key

  ## 1.1 Check for primary key column and auto-increment if possible
  if (pk %nin% names(data)) {
    rids = self$row_ids[[1L]]
    if (!is.integer(rids))
      stopf("Cannot rbind to task: Missing primary key column '%s'", pk)
    data[[pk]] = max(rids) + seq_row(data)
  } else {
    check_new_row_ids(self, data, "disjunct")
  }

  ## 1.2 Check for set equality of column names
  assert_set_equal(names(data), c(self$col_roles$feature, self$col_roles$target, pk))

  ## 1.4 Check that types are matching
  data_col_info = col_info(data, primary_key = pk)
  check_matching_types(self$col_info, data_col_info)

  # 2. Update row_roles
  self$row_roles$use = c(self$row_roles$use, data[[pk]])

  # 3. Update col_info
  self$col_info$levels = Map(union, self$col_info$levels, data_col_info$levels)

  # 4. Overwrite self$backend with new backend
  self$backend = DataBackendRbind$new(self$backend, DataBackendDataTable$new(data, pk))

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

  ## 1.2 Check that there are no duplicated column names
  # assert_disjunct
  tmp = setdiff(intersect(self$col_info$id, names(data)), pk)
  if (length(tmp)) {
    stopf("Cannot cbind task: Duplicated column names: %s", str_collapse(tmp))
  }

  # 2. Overwrite self$backend with new backend
  b2 = DataBackendDataTable$new(data, pk)
  self$backend = DataBackendCbindNew$new(self$backend, b2, unlist(task$col_roles, use.names = FALSE), colnames(data))

  # 3. Update col_info
  data_col_info = col_info(data)
  self$col_info = setkeyv(rbindlist(list(self$col_info, data_col_info[!list(pk)])), "id")
  if (anyDuplicated(self$col_info, by = "id"))
    stopf("Duplicated columns")
  self$col_roles$feature = union(self$col_roles$feature, setdiff(names(data), pk))

  invisible(self)
}

# Performs the following steps to virtually overwrite data in the task:
# 1. Check that an overwrite is feasible
# 2. Overwrite self$backend with new backend (fusion of both backends)
# 3. Update col_info
task_overwrite = function(self, data) {
  assert_data_frame(data, min.rows = 1L, min.cols = 1L)
  data = as.data.table(data)
  pk = self$backend$primary_key

  ## 1.1 Check/Set primary key column
  check_new_row_ids(self, data, "subset")

  ## 1.2 Check that there are no extra column names in data
  tmp = setdiff(names(data), self$col_info$id)
  if (length(tmp))
    stopf("Cannot overwrite task: Extra columns found: %s", str_collapse(tmp))

  ## 1.3 Check that types are matching
  check_matching_types(self$col_info, col_info(data, primary_key = pk))

  # 2. Overwrite Task
  self$backend = DataBackendOverwrite$new(self$backend, DataBackendDataTable$new(data, pk))

  # 3. Update column info
  self$col_info = col_info(self$backend) ### FIXME: we can do better here

  invisible(self)
}

# Performs the following steps to virtually replace columns in the task:
# 1. Check that replacement is feasible
# 2. Overwrite self$backend with new backend (fusion of both backends)
# 3. Update col_info
task_replace_columns = function(self, data) {
  assert_data_frame(data, min.rows = 1L, min.cols = 1L)
  data = as.data.table(data)
  pk = self$backend$primary_key

  ## 1.1 Check/Set primary key column
  check_new_row_ids(self, data, "setequal")

  ## 1.2 Check that there are no extra column names in data
  tmp = setdiff(names(data), self$col_info$id)
  if (length(tmp))
    stopf("Cannot replace columns: Extra columns found: %s", str_collapse(tmp))

  # 2. Overwrite Task
  self$backend = DataBackendReplaceColumns$new(self$backend, DataBackendDataTable$new(data, pk))

  # 3. Update column info
  data_col_info = col_info(data, primary_key = pk)
  self$col_info = setkeyv(rbind(self$col_info[!setdiff(names(data), pk), on = "id"], data_col_info[!pk, on = "id"]), "id")

  invisible(self)
}

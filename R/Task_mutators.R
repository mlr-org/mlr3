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
  if (pk %in% names(data)) {
    check_new_row_ids(self, data, "disjunct")
  } else {
    rids = self$row_ids[[1L]]
    if (is.integer(rids)) {
      data[[pk]] = max(rids) + seq_row(data)
    } else {
      data[[pk]] = sprintf("%s_%i", basename(tempfile("rbind_")), seq_row(data))
    }
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
  assert_data_frame(data, nrows = self$nrow, min.cols = 1L)
  data = as.data.table(data)
  pk = self$backend$primary_key

  ## 1.1 Check primary key column
  if (pk %in% names(data)) {
    check_new_row_ids(self, data, "setequal")
  } else {
    data[[pk]] = self$row_ids[[1L]]
  }

  # 2. Overwrite self$backend with new backend
  b2 = DataBackendDataTable$new(data, pk)
  cols_b1 = unlist(self$col_roles, use.names = FALSE)
  cols_b2 = setdiff(colnames(data), pk)
  self$backend = DataBackendCbind$new(self$backend, b2, cols_b1, cols_b2)

  # 3. Update col_info
  ci = col_info(data)
  self$col_info = ujoin(self$col_info, ci, key = "id")
  self$col_info = rbind(self$col_info, ci[!list(self$col_info$id), on = "id"])
  setkeyv(self$col_info, "id")
  self$col_roles$feature = union(self$col_roles$feature, setdiff(names(data), pk))

  invisible(self)
}

task_replace_features = function(self, data) {
  assert_data_frame(data, nrow = self$nrow, min.cols = 1L)
  data = as.data.table(data)
  pk = self$backend$primary_key

  ## 1.1 Check or create primary key column
  if (pk %in% names(data)) {
    check_new_row_ids(self, data, "setequal")
  } else {
    data[[pk]] = self$row_ids[[1L]]
  }

  # 1.2 Check for target column
  i = which(self$target_names %in% names(data))
  if (length(i))
    stopf("Feature replacement data may not have target column %s", str_collapse(self$target_names[i], quote = "'"))

  # 1.3 Remove old features
  self$col_roles$feature = character(0L)
  keep_cols = unlist(self$col_roles, use.names = FALSE)
  new_features = setdiff(names(data), pk)

  # 2. Cbind new features
  b = DataBackendDataTable$new(data, primary_key = pk)
  self$backend = DataBackendCbind$new(self$backend, b, keep_cols, new_features)
  self$col_roles$feature = new_features

  invisible(self)
}

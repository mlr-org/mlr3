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

#' @include mlr_reflections.R
task_set_col_roles = function(self, private, roles) {
  for (role in c("group", "weight", "name")) {
    if (length(roles[[role]]) > 1L) {
      stopf("There may only be up to one column with role '%s'", role)
    }
  }

  if (inherits(self, "TaskSupervised") && length(roles$target) == 0L) {
    stopf("Supervised tasks need at least one target column")
  }

  private$.col_roles = roles
}

# Performs the following steps to virtually cbind data to the task:
# 1. Check that an cbind is feasible
# 2. Overwrite self$backend with new backend
# 3. Update col_info
task_cbind = function(self, data) {

  assert_data_frame(data)

  if (all(dim(data) == 0L)) {
    return(invisible(self))
  }

  # 1. Check that an cbind is feasible
  data = as.data.table(data)
  pk = self$backend$primary_key
  rn = self$backend$rownames

  ## 1.1 Check primary key column
  if (pk %in% names(data)) {
    assert_integerish(data[[pk]], any.missing = FALSE, unique = TRUE)
    if (!setequal(data[[pk]], rn)) {
      stopf("Cannot mutate task: row ids not set-equal to existing row ids")
    }
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
  self$col_roles$feature = setdiff(union(self$col_roles$feature, names(data)), c(pk, self$col_roles$target))

  invisible(self)
}

task_rename = function(self, old, new) {
  self$backend = DataBackendRename$new(self$backend, old, new)
  setkeyv(self$col_info[old, ("id") := new, on = "id"], "id")
  self$col_roles = map(self$col_roles, map_values, old = old, new = new)
  invisible(self)
}

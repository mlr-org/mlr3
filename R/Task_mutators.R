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


task_rename = function(self, old, new) {
  self$backend = DataBackendRename$new(self$backend, old, new)
  setkeyv(self$col_info[old, ("id") := new, on = "id"], "id")
  self$col_roles = map(self$col_roles, map_values, old = old, new = new)
  invisible(self)
}

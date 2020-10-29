# old, deprecated, will be removed soon
task_set_row_role = function(self, private, rows, new_roles, exclusive = TRUE) {
  rows = assert_row_ids(rows)
  assert_subset(rows, self$backend$rownames)
  assert_subset(new_roles, mlr_reflections$task_row_roles)
  assert_flag(exclusive)

  for (role in new_roles) {
    private$.row_roles[[role]] = union(private$.row_roles[[role]], rows)
  }

  if (exclusive) {
    for (role in setdiff(names(self$row_roles), new_roles)) {
      private$.row_roles[[role]] = setdiff(private$.row_roles[[role]], rows)
    }
  }
}

# old, deprecated, will be removed soon
task_set_col_role = function(self, private, cols, new_roles, exclusive = TRUE) {
  assert_character(cols, any.missing = FALSE)
  assert_subset(cols, self$col_info$id)
  assert_subset(new_roles, mlr_reflections$task_col_roles[[self$task_type]])
  assert_flag(exclusive)

  col_roles = self$col_roles

  for (role in new_roles) {
    col_roles[[role]] = union(col_roles[[role]], cols)
  }

  if (exclusive) {
    for (role in setdiff(names(col_roles), new_roles)) {
      col_roles[[role]] = setdiff(col_roles[[role]], cols)
    }
  }

  if (inherits(self, "TaskSupervised") && length(col_roles[["target"]]) == 0L) {
    stopf("Supervised tasks need a target column")
  }

  for (role in c("group", "weight")) {
    if (length(col_roles[[role]]) > 1L)
      stopf("Multiple columns with role '%s' not supported", role)
  }

  role = c("stratum", "group", "weight")
  property = c("strata", "groups", "weights")
  for (i in seq_along(role)) {
    n = length(col_roles[[role[i]]])
    if (n == 0L) {
      private$.properties = setdiff(private$.properties, property[i])
    } else if (n == 1L) {
      private$.properties = union(private$.properties, property[i])
    }
  }

  self$col_roles = col_roles
}

task_set_col_roles = function(private, cols, roles = NULL, add_to = NULL, remove_from = NULL) {
  roles = add_to = remove_from = NULL
  add_to = 1
  not_null = which(!c(is.null(roles), is.null(add_to), is.null(remove_from)))
  if (length(not_null) != 1L) {
    stopf("Exactly one argument of 'roles', 'add_to' and 'remove_from' must be provided")
  }

  switch(not_null,
    { private$.col_roles = imap(private$.col_roles, function(elements, role) {
        if (role %in% roles) union(elements, cols) else setdiff(elements, cols)
      })
    },

    for (role in add_to)
      private$.col_roles[[role]] = union(private$.col_roles[[role]], cols)
    ,

    for (role in remove_froms)
      private$.col_roles[[role]] = setdiff(private$.col_roles[[role]], cols)
  )
}

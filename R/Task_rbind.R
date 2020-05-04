task_rbind = function(backend, task) {
  pk = task$backend$primary_key
  rn = task$backend$rownames
  pk_in_backend = TRUE
  type_check = TRUE

  if (is.data.frame(backend)) {
    pk_in_backend = pk %in% names(backend)
    type_check = FALSE # done by auto-converter

    keep_cols = intersect(names(backend), task$backend$colnames)
    if (length(keep_cols) == pk_in_backend || nrow(backend) == 0L) {
      return(invisible(task))
    }

    if (!pk_in_backend) {
      start = if (length(rn)) max(rn) + 1L else 1L
      pk = seq(from = start, to = start + nrow(backend) - 1L)
    }

    ci = task$col_info[list(keep_cols), on = "id"]
    backend = do.call(data.table, Map(auto_convert,
      value = as.list(backend)[ci$id],
      id = ci$id, type = ci$type, levels = ci$levels))

    backend = as_data_backend(backend, primary_key = pk)
  } else {
    assert_backend(backend)
    if (backend$ncol <= 1L || backend$nrow == 0L) {
      return(invisible(task))
    }
  }

  if (pk_in_backend && any(backend$rownames %in% task$backend$rownames)) {
    stopf("Cannot rbind data to task '%s', duplicated row ids", task$id)
  }

  # columns with these roles must be present in data
  mandatory_roles = c("target", "feature", "weight")
  mandatory_cols = unlist(task$col_roles[mandatory_roles], use.names = FALSE)
  missing_cols = setdiff(mandatory_cols, backend$colnames)
  if (length(missing_cols)) {
    stopf("Cannot rbind data to task '%s', missing the following mandatory columns: %s", task$id, str_collapse(missing_cols))
  }

  # merge col infos
  tab = merge(task$col_info, col_info(backend), by = "id",
    all.x = TRUE, all.y = FALSE, suffixes = c("", ".y"))
  levels = levels.y = type = type.y = NULL

  # type check
  if (type_check) {
    ii = head(tab[type != type.y, which = TRUE], 1L)
    if (length(ii)) {
      stopf("Cannot rbind to task: Types do not match for column: %s (%s != %s)", tab$id[ii], tab$type[ii], tab$type.y[ii])
    }
  }

  # merge factor levels
  vunion = function(x, y) Map(union, x, y)
  tab[type %in% c("factor", "ordered"), levels := list(vunion(levels, levels.y))]
  tab[, c("type.y", "levels.y") := list(NULL, NULL)]

  # everything looks good, modify task
  task$backend = DataBackendRbind$new(task$backend, backend)
  task$col_info = tab
  task$row_roles$use = c(task$row_roles$use, backend$rownames)

  invisible(task)
}

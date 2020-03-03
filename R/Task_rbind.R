task_rbind = function(backend, task) {
  UseMethod("task_rbind")
}

task_rbind.data.frame = function(backend, task) {
  if (any(dim(backend) == 0L)) {
    return(invisible(task))
  }

  backend = as.data.table(backend)
  pk = task$backend$primary_key
  rn = task$backend$rownames

  if (pk %nin% names(backend)) {
    start = if (length(rn)) max(rn) + 1L else 1L
    pk = seq(from = start, to = start + nrow(backend) - 1L)
  }

  ci = task$col_info[list(names(backend)), on = "id", nomatch = NULL]
  pmap(ci, function(id, type, levels) {
    value = backend[[id]]
    cl = class(value)[1L]

    if (type %in% c("factor", "ordered")) {
      newlevels = union(levels, if (is.factor(value)) levels(value) else unique(value))
      value = as_factor(value, levels = newlevels, ordered = (type == "ordered"))
      set(backend, j = id, value = value)
    } else if (type != cl) {
      if (allMissing(value)) {
        storage.mode(value) = type
        set(backend, j = id, value = value)
      } else {
        stopf("Cannot rbind to task: Types do not match for column: %s (%s != %s)", id, type, cur_type)
      }
    }
  })

  task_rbind(as_data_backend(backend, primary_key = pk), task)
}

task_rbind.DataBackend = function(backend, task) {
  if (backend$ncol <= 1L) {
    return(invisible(task))
  }

  if (any(backend$rownames %in% task$backend$rownames)) {
    stopf("Cannot rbind data to task '%s', duplicated row ids", task$id)
  }

  # columns with these roles must be present in data
  mandatory_roles = c("target", "feature", "group", "stratum", "order", "weight")
  mandatory_cols = unlist(task$col_roles[mandatory_roles], use.names = FALSE)
  missing_cols = setdiff(mandatory_cols, backend$colnames)
  if (length(missing_cols)) {
    stopf("Cannot rbind data to task '%s', missing the following mandatory columns: %s", task$id, str_collapse(missing_cols))
  }

  vunion = function(x, y) Map(union, x, y)
  tab = merge(task$col_info, col_info(backend), by = "id", all.x = TRUE, all.y = FALSE, suffixes = c("", ".y"))
  ii = head(tab[type != type.y, which = TRUE], 1L)
  if (length(ii)) {
    stopf("Cannot rbind to task: Types do not match for column: %s (%s != %s)", tab$id[ii], tab$type[ii], tab$type.y[ii])
  }

  tab[get("type") %in% c("factor", "ordered"), "levels" := list(vunion(levels, levels.y))]
  tab[, c("type.y", "levels.y") := list(NULL, NULL)]

  task$backend = DataBackendRbind$new(task$backend, backend)
  task$col_info = tab
  task$row_roles$use = c(task$row_roles$use, backend$rownames)

  invisible(task)
}

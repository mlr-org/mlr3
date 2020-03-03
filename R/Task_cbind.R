task_cbind = function(backend, task) {
  UseMethod("task_cbind")
}


task_cbind.data.frame = function(backend, task) {
  if (any(dim(backend) == 0L)) {
    return(invisible(task))
  }

  pk = task$backend$primary_key
  row_ids = if (pk %in% names(backend)) pk else task$backend$rownames

  backend = as_data_backend(backend, primary_key = row_ids)
  task_cbind(backend, task)
}

task_cbind.DataBackend = function(backend, task) {
  if (backend$ncol <= 1L) {
    return(invisible(task))
  }

  assert_set_equal(backend$rownames, task$backend$rownames)

  task$backend = DataBackendCbind$new(task$backend, backend)
  ci = col_info(backend)
  task$col_info = ujoin(task$col_info, ci, key = "id")
  task$col_info = rbind(task$col_info, ci[!list(task$col_info), on = "id"])
  setkeyv(task$col_info, "id")

  task$col_roles$feature = union(task$col_roles$feature, setdiff(backend$colnames, c(task$backend$primary_key, task$col_roles$target)))

  invisible(task)
}

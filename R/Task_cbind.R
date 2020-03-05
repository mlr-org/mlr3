task_cbind = function(backend, task) {
  pk = task$backend$primary_key

  if (is.data.frame(backend)) {
    if (any(dim(backend) == 0L)) {
      return(invisible(task))
    }

    row_ids = if (pk %in% names(backend)) pk else task$row_ids
    backend = as_data_backend(backend, primary_key = row_ids)
  } else {
    assert_backend(backend)
    if (backend$ncol <= 1L) {
      return(invisible(task))
    }
  }

  assert_set_equal(backend$rownames, task$row_ids)
  ci = col_info(backend)

  # update col info
  task$col_info = ujoin(task$col_info, ci, key = "id")
  task$col_info = rbind(task$col_info, ci[!list(task$col_info), on = "id"])
  setkeyv(task$col_info, "id")

  # add new features
  task$col_roles$feature = union(task$col_roles$feature, setdiff(backend$colnames, c(task$backend$primary_key, task$col_roles$target)))

  # update backend
  task$backend = DataBackendCbind$new(task$backend, backend)

  invisible(task)
}

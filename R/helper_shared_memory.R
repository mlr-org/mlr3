# Move the data of all DataBackendDataTable objects of `tasks` into OS-level shared memory
# via package mori, if enabled via option "mlr3.shared_memory".
# Shared data serializes as a small reference instead of a full copy,
# so parallel workers on the same machine map the data instead of receiving a serialized copy.
# Returns a list of entries for unshare_backends(), which must be called before the results
# are returned to the user, because shared references do not survive the R session.
share_backends = function(tasks) {
  if (!isTRUE(getOption("mlr3.shared_memory", FALSE))) {
    return(list())
  }
  if (!require_namespaces("mori", quietly = TRUE)) {
    warning_config("Option 'mlr3.shared_memory' is set, but package 'mori' is not installed")
    return(list())
  }

  restore = list()
  for (task in tasks) {
    backend = task$backend
    if (!inherits(backend, "DataBackendDataTable")) {
      next
    }
    private = get_private(backend)
    data = private$.data
    if (mori::is_shared(data)) {
      next
    }
    # trigger the hash calculation so the cached value is computed from the unshared data
    backend$hash
    shared = tryCatch(mori::share(data), error = function(e) {
      lg$warn("Cannot move data of task '%s' into shared memory: %s", task$id, conditionMessage(e))
      NULL
    })
    if (is.null(shared)) {
      next
    }
    private$.data = shared
    restore[[length(restore) + 1L]] = list(backend = backend, data = data)
  }
  restore
}

# Restore the original data of the backends shared by share_backends(),
# so that returned result objects do not reference shared memory regions.
unshare_backends = function(restore) {
  for (entry in restore) {
    private = get_private(entry$backend)
    private$.data = entry$data
  }
}

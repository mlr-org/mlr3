cache_get = function(cache, key, value) {
  if (exists(key, envir = cache, inherits = FALSE))
    return(get(key, envir = cache))
  assign(key, value, envir = cache)
  return(value)
}

cache_clear = function(cache, keys = NULL) {
  if (is.null(keys)) {
    rm(list = ls(cache, all.names = TRUE), envir = cache)
  } else {
    remove(cache, keys)
  }
}

clone_env = function(src) {
  list2env(as.list(src, all.names = TRUE), parent = emptyenv())
}

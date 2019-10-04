task_types = function(x) {
  unique(map_chr(unname(x), "task_type"))
}

hashes = function(x) {
  map_chr(unname(x), "hash")
}

hash = function(...) {
  digest::digest(list(...), algo = "xxhash64")
}

# updating join:
# replaces values in x with values in y
ujoin = function(x, y, key) {
  cn = setdiff(intersect(names(x), names(y)), key)
  expr = parse(text = paste0("`:=`(", paste0(sprintf("%1$s=i.%1$s", cn), collapse = ","), ")"))
  x[y, eval(expr), on = key]
}

translate_types = function(x) {
  r_types = mlr_reflections$task_feature_types
  p_types = names(mlr_reflections$task_feature_types)
  factor(map_values(x, r_types, p_types), levels = p_types)
}

allow_partial_matching = list(
  warnPartialMatchArgs = FALSE,
  warnPartialMatchAttr = FALSE,
  warnPartialMatchDollar = FALSE
)

# determines if execution via future will be running locally or remotely
use_future = function() {
  if (!isNamespaceLoaded("future") || inherits(future::plan(), "uniprocess")) {
    return(FALSE)
  }

  if (!requireNamespace("future.apply", quietly = TRUE)) {
    lg$warn("Package 'future.apply' could not be loaded. Parallelization disabled.")
    return(FALSE)
  }

  return(TRUE)
}

# inverts a named list:
# elements become names, names become elements
invert = function(x, lvls) {
  f = factor(unlist(x, use.names = FALSE), levels = lvls)
  split(rep(names(x), lengths(x)), f, drop = FALSE)
}

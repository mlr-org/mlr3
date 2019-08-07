ids = function(x) {
  map_chr(unname(x), "id")
}

hashes = function(x) {
  map_chr(unname(x), "hash")
}

# updating join:
# replaces values in x with values in y
ujoin = function(x, y, key) {
  cn = setdiff(intersect(names(x), names(y)), key)
  expr = parse(text = paste0("`:=`(", paste0(sprintf("%1$s=i.%1$s", cn), collapse = ","), ")"))
  x[y, eval(expr), on = key]
}

hash = function(...) {
  digest::digest(list(...), algo = "xxhash64")
}

hash_resample_iteration = function(task, learner, resampling) {
  hash(task$hash, learner$hash, resampling$hash)
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

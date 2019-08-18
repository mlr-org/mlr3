ids = function(x) {
  map_chr(unname(x), "id")
}

task_types = function(x) {
  unique(map_chr(unname(x), "task_type"))
}

hashes = function(x) {
  map_chr(unname(x), "hash")
}

hash = function(...) {
  digest::digest(list(...), algo = "xxhash64")
}

#' @title Calculate the Hash for a ResampleResult
#'
#' @description
#' Internally used, exported for addon-packages.
#'
#' @param task :: [Task].
#' @param learner :: [Learner].
#' @param resampling :: [Resampling].
#' @return (`character(1)`) hash value.
#' @keywords internal
#' @export
hash_resample_result = function(task, learner, resampling) {
  hash(task$hash, learner$hash, resampling$hash)
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

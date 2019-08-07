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


distinct = function(x, drop = TRUE, na_rm = TRUE) {
  assert_flag(drop)
  assert_flag(na_rm)
  UseMethod("distinct")
}

distinct.default = function(x, drop = TRUE, na_rm = TRUE) {
  lvls = unique(x)
  if (na_rm)
    lvls = lvls[!is.na(lvls)]
  lvls
}

distinct.logical = function(x, drop = TRUE, na_rm = TRUE) {
  if (!drop) {
    lvls = c(TRUE, FALSE)
    if (!na_rm && anyMissing(x))
      lvls = c(lvls, NA)
  } else {
    lvls = unique(x)
  }
  lvls
}

distinct.factor = function(x, drop = TRUE, na_rm = TRUE) {
  if (drop) {
    lvls = as.character(unique(x))
    if (na_rm)
      lvls = lvls[!is.na(lvls)]
  } else {
    lvls = levels(x)
    if (!na_rm && anyMissing(x))
      lvls = c(lvls, NA_character_)
  }
  lvls
}

filter_oob_index = function(x, lower, upper) {
  x = assert_integerish(x, coerce = TRUE)
  x[!is.na(x) & x >= lower & x <= upper]
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

# TODO: move to mlr3misc
set_col_names = function (x, nm = x, ...) {
    if (is.function(nm)) {
      nm = map_chr(names2(x), nm)
    }
    colnames(x) = nm
    x
}

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

distinct = function(x, drop = TRUE) {
  if (is.logical(x) && !drop) {
    lvls = c(FALSE, TRUE)
  } else if (is.factor(x)) {
    lvls = levels(x)
    if (drop)
      lvls = lvls[lvls %in% x]
  } else {
    lvls = unique(x)
    lvls = lvls[!is.na(lvls)]
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

translate_types = function(x) {
  r_types = mlr_reflections$task_feature_types
  p_types = names(mlr_reflections$task_feature_types)
  factor(map_values(x, r_types, p_types), levels = p_types)
}

rbind_named = function(x, y) {
  assert_matrix(x)
  assert_matrix(y)
  assert_names(colnames(x), permutation.of = colnames(y))

  rbind(x, y[, match(colnames(x), colnames(y)), drop = FALSE])
}

# converts to factor, and ensures that levels are in the right order
as_factor = function(x, levels, ...) {
  if (is.character(x)) {
    x = factor(x, levels = levels)
    assert_factor(x, ...)
  } else {
    assert_factor(x, levels = levels, ...)
    if (!identical(levels(x), levels))
      x = factor(x, levels = levels)
  }
  x
}

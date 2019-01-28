ids = function(x) {
  map_chr(unname(x), "id")
}

hashes = function(x) {
  map_chr(unname(x), "hash")
}

did_you_mean = function(str, candidates) {
  candidates = unique(candidates)
  D = set_names(adist(str, candidates, ignore.case = TRUE, partial = TRUE)[1L, ], candidates)
  suggested = names(head(sort(D[D <= ceiling(0.2 * nchar(str))]), 3L))

  if (!length(suggested))
    return("")
  sprintf(" Did you mean %s?", str_collapse(suggested, quote = "'", sep = " / "))
}

# updating join:
# replaces values in x with values in y
ujoin = function(x, y, key) {
  cn = setdiff(intersect(names(x), names(y)), key)
  expr = parse(text = paste0("`:=`(", paste0(sprintf("%1$s=i.%1$s", cn), collapse = ","), ")"))
  x[y, eval(expr), on = key]
}

distinct = function(x) {
  lvls = if (is.factor(x)) as.character(unique(x)) else unique(x)
  lvls[!is.na(lvls)]
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

generate_formula = function(lhs, rhs) {
  if (length(lhs) == 0L)
    lhs = NULL
  if (length(rhs) == 0L)
    rhs = "1"
  f = reformulate(rhs, response = lhs)
  environment(f) = NULL
  f
}

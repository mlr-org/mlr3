ids = function(x) {
  map_chr(unname(x), "id")
}

hashes = function(x) {
  map_chr(unname(x), "hash")
}

stri_wrap = function(str, initial, n = 100L) {
  str = if (length(str) == 0L) "-" else paste0(head(str, n), collapse = ", ")
  strwrap(str, initial = initial, exdent = 2L)
}

did_you_mean = function(str, candidates) {
  candidates = unique(candidates)
  D = set_names(adist(str, candidates, ignore.case = TRUE, partial = TRUE)[1L, ], candidates)
  suggested = names(head(sort(D[D <= ceiling(0.2 * nchar(str))]), 3L))

  if (length(suggested)) sprintf(" Did you mean %s?", paste0("'", suggested, "'", collapse = " / ")) else ""
}

# updating join:
# replaces values in x with values in y
ujoin = function(x, y, key) {
  cn = setdiff(intersect(names(x), names(y)), key)
  expr = parse(text = paste0("`:=`(", paste0(sprintf("%1$s=i.%1$s", cn), collapse = ","), ")"))
  x[y, eval(expr), on = key]
}

distinct = function(x) {
  if (is.factor(x)) levels(x) else unique(x)
}

# rounds numbers randomly, depending on their decimal digits:
# 1.3 is rounded down with probability 70%, and rounded up with probability 30%
# used for resampling / stratification
rround = function(x) {
  xi = as.integer(x)
  delta = x - xi
  ii = which(delta > sqrt(.Machine$double.eps))
  if (length(ii))
    xi[ii] = xi[ii] + (delta[ii] > runif(length(ii)))
  xi
}

filter_oob_index = function(x, lower, upper) {
  x = assert_integerish(x, coerce = TRUE)
  x[!is.na(x) & x >= lower & x <= upper]
}

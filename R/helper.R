# @title Extract the id slot
#
# @description
# A simple helper which extracts the id slot of a list of objects.
#
# @param x \[`list()`\]\cr
#  List of objects which have a slot named `"id"`.
#
# @return \[`character`\] of the same length as input `x`.
# @export
# @examples
# x = mlr_tasks$mget(c("iris", "sonar"))
# ids(x)
ids = function(x) {
  map_chr(unname(x), "id")
}

# same as ids() but for hashes
hashes = function(x) {
  map_chr(unname(x), "hash")
}

stri_wrap = function(str, initial, n = 100L) {
  str = if (length(str) == 0L) "-" else paste0(head(str, n), collapse = ", ")
  strwrap(str, initial = initial, exdent = 2L)
}

did_you_mean = function(str, candidates) {
  candidates = unique(candidates)
  D = setNames(adist(str, candidates, ignore.case = TRUE, partial = TRUE)[1L, ], candidates)
  suggested = names(head(sort(D[D <= ceiling(0.2 * nchar(str))]), 3L))

  if (length(suggested)) sprintf(" Did you mean %s?", paste0("'", suggested, "'", collapse = " / ")) else ""
}

info = function(msg, ...) {
  if (isTRUE(getOption("mlr3.verbose")))
    messagef(msg, ...)
}

debug = function(msg, ...) {
  if (isTRUE(getOption("mlr3.debug", FALSE)))
    message("[debug] ", sprintf(msg, ...))
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

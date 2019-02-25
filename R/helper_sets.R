`%fnin%` = function(x, y) {
  !fmatch(x, y, 0L)
}

funique = function(x, is_unique = FALSE) {
  if (is.null(attr(x, ".unique"))) {
    if (!isTRUE(is_unique))
      x = unique(x)
    attr(x, ".unique") = TRUE
  }
  x
}

set_union = function(x, y) {
  funique(c(x, y))
}

set_equal = function(x, y) {
  !(anyMissing(fmatch(x, y)) || anyMissing(fmatch(y, x)))
}

set_intersect = function(x, y) {
  is_hashed = function(x) !is.null(attr(x, ".match.hash"))

  if (is_hashed(y) || !is_hashed(x))
    funique(y[fmatch(x, y, 0L)], attr(x, ".unique"))
  else
    funique(x[fmatch(y, x, 0L)], attr(y, ".unique"))
}

set_diff = function(x, y) {
  if (length(x) == 0L && length(y) == 0L)
    return(x)
  funique(x[fmatch(x, y, 0L) == 0L], attr(x, ".unique"))
}

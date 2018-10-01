rcbind = function(x, y) {
  if (ncol(x) == 0L)
    return(y)

  if (ncol(y) == 0L)
    return(x)

  if (nrow(x) != nrow(y))
    stopf("Tables have different number of rows (%s: %i, %s: %i)",
      deparse(substitute(x)), nrow(x), deparse(substitute(y)), nrow(y))

  ii = wf(names(x) %in% names(y))
  if (length(ii))
    stopf("Duplicated names: %s", names(x)[ii])

  x[, names(y) := y]
}

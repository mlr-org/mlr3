rcbind = function(x, y) {
  if (ncol(x) == 0L)
    return(y)

  if (ncol(y) == 0L)
    return(x)

  if (nrow(x) != nrow(y))
    stopf("Tables have different number of rows (x: %i, y: %i)",
      nrow(x), nrow(y))

  ii = wf(names(x) %in% names(y))
  if (length(ii))
    stopf("Duplicated names: %s", names(x)[ii])

  x[, names(y) := y][]
}

flatten = function(x, cols) {
  for (col in intersect(cols, names(x))) {
    tmp = rbindlist(x[[col]], fill = TRUE)
    x[, (col) := NULL]
    rcbind(x, tmp)
  }
  x[]
}

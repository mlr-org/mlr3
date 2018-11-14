# cbind by reference
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

# updating join:
# replaces values in x with values in y
ujoin = function(x, y, key) {
  cn = setdiff(intersect(names(x), names(y)), key)
  expr = parse(text = paste0("`:=`(", paste0(sprintf("%1$s=i.%1$s", cn), collapse = ","), ")"))
  x[y, eval(expr), on = key]
}

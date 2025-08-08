# extract values from a single column of a data table
# tries to avoid the overhead of data.table for small tables
# returns a vector sorted by the order of i
# if the key column is not unique, the first match is returned
# if no-matching elements are found, NA is returned
fget = function(tab, i, j, key) {
  if (nrow(tab) > 1000L) {
    ijoin(tab, i, j, key, mult = "first", nomatch = NA)[[1L]]
  } else {
    x = tab[[key]]
    tab[[j]][match(i, x)]
  }
}

ijoin = function(tab, .__i__, .__j__, .__key__, nomatch = NULL, mult = "all") {
  if (!is.list(.__i__)) {
    .__i__ = list(.__i__)
  }
  tab[.__i__, .__j__, with = FALSE, nomatch = nomatch, on = .__key__, mult = mult]
}

# updating join:
# replaces values in x with values in y
ujoin = function(x, y, key) {
  cn = setdiff(intersect(names(x), names(y)), key)
  expr = parse(text = paste0("`:=`(", paste0(sprintf("%1$s=i.%1$s", cn), collapse = ","), ")"))
  x[y, eval(expr), on = key][]
}

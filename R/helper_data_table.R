# extract values from a single column of a data table
# tries to avoid the overhead of data.table for small tables
fget = function(tab, i, j, key) {
  if (nrow(tab) > 1000L) {
    ijoin(tab, i, j, key)[[1L]]
  } else {
    x = tab[[key]]
    if (is.character(x) && is.character(i)) {
      tab[[j]][x %chin% i]
    } else {
      tab[[j]][x %in% i]
    }
  }
}

ijoin = function(tab, .__i__, .__j__, .__key__) {
  if (!is.list(.__i__)) {
    .__i__ = list(.__i__)
  }
  tab[.__i__, .__j__, with = FALSE, nomatch = NULL, on = .__key__]
}

# updating join:
# replaces values in x with values in y
ujoin = function(x, y, key) {
  cn = setdiff(intersect(names(x), names(y)), key)
  expr = parse(text = paste0("`:=`(", paste0(sprintf("%1$s=i.%1$s", cn), collapse = ","), ")"))
  x[y, eval(expr), on = key][]
}

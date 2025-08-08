# extract values from a single column `j` of a data table where the `key` column matches `i`
# tries to avoid the overhead of data.table for small tables
# i must be a single value, use fget_keys for multiple values
# returns all matches
# if `i` has no match in `key`, `NULL` is returned
fget_key = function(tab, i, j, key) {
  if (nrow(tab) > 1000L) {
    ijoin(tab, i, j, key, mult = "all", nomatch = NULL)[[1L]]
  } else {
    x = tab[[key]]
    if (is.character(x) && is.character(i)) {
      tab[[j]][x %chin% i]
    } else {
      tab[[j]][x %in% i]
    }
  }
}

# extract values from a single column `j` of a data table where the `key` column matches `i`
# tries to avoid the overhead of data.table for small tables
# i can be a vector of values
# returns a vector sorted by the order of i
# if the key column is not unique, the first match is returned for each i
# if no-matching elements are found, NA is returned
fget_keys = function(tab, i, j, key) {
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

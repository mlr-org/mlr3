is_scalar_na = function(x) {
  is.vector(x) && length(x) == 1L && is.na(x)
}

"%nin%" = function(x, y) {
  !match(x, y, nomatch = 0L)
}

require_namespaces = function(pkgs, msg = "The following packages are missing: %s") {
  ok = vlapply(unique(pkgs), requireNamespace, quietly = TRUE)
  if (!all(ok))
    stopf(msg, paste0(pkgs[!ok], collapse = ","))
}

shuffle = function(x, n = length(x), ...) {
  x[sample.int(length(x), n, ...)]
}

named_list = function(nn, init = NULL) {
  x = vector("list", length(nn))
  if (!is.null(init))
    x[] = list(init)
  setNames(x, nn)
}

distinct = function(x) {
  if (is.factor(x)) levels(x) else unique(x)
}

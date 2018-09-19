"%nin%" = function(x, y) {
  !match(x, y, nomatch = 0L)
}

require_namespaces = function(pkgs, msg = "The following packages are missing: %s") {
  ok = vlapply(unique(pkgs), requireNamespace, quietly = TRUE)
  if (!all(ok))
    stopf(msg, paste0(pkgs[!ok], collapse = ","))
}

shuffle = function(x) {
  # a "safe" sample() for n == length(x)
  if (length(x) <= 1L)
    return(x)
  sample(x)
}

named_list = function(nn) {
  setNames(vector("list", length(nn)), nn)
}

distinct = function(x) {
  if (is.factor(x)) levels(x) else unique(x)
}

symdiff = function(x, y) {
  unique(c(setdiff(x, y), setdiff(y, x)))
}

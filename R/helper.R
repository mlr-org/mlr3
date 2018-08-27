require_namespaces = function(pkgs, msg = "The following packages are missing: %s") {
  ok = vlapply(unique(pkgs), requireNamespace, quietly = TRUE)
  if (!all(ok))
    stopf(msg, stri_flatten(pkgs[!ok], ","))
}

ids = function(x) {
  vcapply(x, "[[", "id")
}

shuffle = function(x) {
  # a "safe" sample() for n == length(x)
  if (length(x) <= 1L)
    return(x)
  sample(x)
}

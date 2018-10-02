stri_describe = function(name, str, n = 100L) {
  str = if (length(str) == 0L) "-" else paste0(head(str, n), collapse = ", ")
  strwrap(str, initial = name, exdent = 2L)
}

stri_head = function(str, n = 3L, sep = " ", collapse = ", ") {
  x = paste(head(str, n), sep = sep, collapse = collapse)
  if (length(str) > n)
    x = paste(x, "[...]", sep = sep)
  x
}

stri_suggest = function(str, candidates = character(0L), n = 3L) {
  n = min(n, length(candidates))
  if (n == 0L)
    return(character(0L))

  d = setNames(adist(str, candidates, ignore.case = TRUE, partial = TRUE)[1L, ], candidates)
  head(names(d[d < 0.2 * nchar(str)]), n)
}

stri_key_val = function(x) {
  paste0(paste(names(x), x, sep = "="), collapse = ", ")
}

stri_list = function(initial, str, n = 100L) {
  str = if (length(str) == 0L) "-" else paste0(head(str, n), collapse = ", ")
  strwrap(str, initial = initial, exdent = 2L)
}

stri_peek = function(str, sep = " ", collapse = ", ", n = 10L) {
  x = paste(head(str, n), sep = sep, collapse = collapse)
  if (length(str) > n)
    x = paste(paste(x, collapse = collapse), "[...]")
  x
}

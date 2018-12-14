stri_wrap = function(str, initial, n = 100L) {
  str = if (length(str) == 0L) "-" else paste0(head(str, n), collapse = ", ")
  strwrap(str, initial = initial, exdent = 2L)
}


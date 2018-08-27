stri_list = function(initial, str, n = 100L) {
  str = if (length(str) == 0L) "-" else stri_flatten(head(str, n), ", ")
  stri_wrap(str, initial = initial, exdent = 2L)
}

stri_peek = function(str, sep = " ", collapse = ", ", n = 10L) {
  x = stringi::stri_paste(head(str, n), sep = sep, collapse = collapse)
  if (length(str) > n)
    x = stri_join(x, collapse, "[...]")
  x
}

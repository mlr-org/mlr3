stri_key_val = function(x) {
  paste0(paste(names(x), x, sep = "="), collapse = ", ")
}

stri_wrap = function(str, initial, n = 100L) {
  str = if (length(str) == 0L) "-" else paste0(head(str, n), collapse = ", ")
  strwrap(str, initial = initial, exdent = 2L)
}

stri_head = function(str, n = 10L, collapse = ", ", quote = "'") {
  formatted = head(str, n)
  if (nzchar(quote))
    formatted = paste0(quote, formatted, quote)
  formatted = paste(formatted, collapse = collapse)
  if (length(str) > n)
    formatted = paste(formatted, "[...]")
  formatted
}

stri_suggest = function(str, candidates = character(0L), n = 3L) {
  n = min(n, length(candidates))
  if (n == 0L)
    return(character(0L))

  d = setNames(adist(str, candidates, ignore.case = TRUE, partial = TRUE)[1L, ], candidates)
  head(names(d[d < 0.2 * nchar(str)]), n)
}

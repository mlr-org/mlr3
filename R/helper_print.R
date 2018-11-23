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

did_you_mean = function(str, candidates) {
  candidates = unique(candidates)
  D = setNames(adist(str, candidates, ignore.case = TRUE, partial = TRUE)[1L, ], candidates)
  suggested = names(head(sort(D[D <= ceiling(0.2 * nchar(str))]), 3L))

  if (length(suggested)) sprintf(" Did you mean %s?", paste0("'", suggested, "'", collapse = " / ")) else ""
}

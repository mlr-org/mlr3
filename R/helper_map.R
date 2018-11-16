map = function(.x, .f, ..., .use.names = TRUE) {
  sapply(.x, .f, ..., simplify = FALSE, USE.NAMES = .use.names)
}

map_lgl = function(.x, .f, ..., .use.names = TRUE) {
  vapply(.x, .f, NA, ..., USE.NAMES = .use.names)
}

map_int = function(.x, .f, ..., .use.names = TRUE) {
  vapply(.x, .f, NA_integer_, ..., USE.NAMES = .use.names)
}

map_dbl = function(.x, .f, ..., .use.names = TRUE) {
  vapply(.x, .f, NA_real_, ..., USE.NAMES = .use.names)
}

map_chr = function(.x, .f, ..., .use.names = TRUE) {
  vapply(.x, .f, NA_character_, ..., USE.NAMES = .use.names)
}

map_dtr = function(.x, .f, ..., .fill = FALSE, .key = NULL) {
  out = rbindlist(lapply(.x, .f, ...), fill = .fill)
  if (!is.null(.key))
    setkeyv(out, .key)
  out
}

map_dtc = function(.x, .f, ..., .key = NULL) {
  out = as.data.table(.x)
  if (!is.null(.key))
    setkeyv(out, .key)
  out
}

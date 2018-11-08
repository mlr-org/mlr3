# This is partly taken from compat-purrr (last updated: rlang 0.3.0)
# https://github.com/r-lib/rlang/blob/master/R/compat-purrr.R

map = function(.x, .f, ...) {
  lapply(.x, .f, ...)
}

map_mold = function(.x, .f, .mold, ...) {
  out = vapply(.x, .f, .mold, ..., USE.NAMES = FALSE)
  names(out) = names(.x)
  out
}

map_lgl = function(.x, .f, ...) {
  map_mold(.x, .f, NA, ...)
}

map_int = function(.x, .f, ...) {
  map_mold(.x, .f, NA_integer_, ...)
}

map_dbl = function(.x, .f, ...) {
  map_mold(.x, .f, NA_real_, ...)
}

map_chr = function(.x, .f, ...) {
  map_mold(.x, .f, NA_character_, ...)
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

pluck = function(.x, .f) {
  map(.x, `[[`, .f)
}

pluck_lgl = function(.x, .f) {
  map_lgl(.x, `[[`, .f)
}

pluck_int = function(.x, .f) {
  map_int(.x, `[[`, .f)
}

pluck_dbl = function(.x, .f) {
  map_dbl(.x, `[[`, .f)
}

pluck_chr = function(.x, .f) {
  map_chr(.x, `[[`, .f)
}

probe = function(.x, .p, ...) {
  if (is.logical(.p)) {
    stopifnot(length(.p) == length(.x))
    .p
  } else {
    map_lgl(.x, .p, ...)
  }
}

keep = function(.x, .f, ...) {
  sel = probe(.x, .f, ...)
  .x[!is.na(sel) & sel]
}

discard = function(.x, .p, ...) {
  sel = probe(.x, .p, ...)
  .x[is.na(sel) | !sel]
}

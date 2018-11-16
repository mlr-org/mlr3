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

assert_id = function(id) {
  assert_string(id, min.chars = 1L)
}

assert_packages = function(packages) {
  assert_character(packages, any.missing = FALSE, min.chars = 1L, unique = TRUE)
}


assert_par_set = function(par_set) {
  assert_r6(par_set, "ParamSet")
}

assert_par_vals = function(par_vals, par_set) {
  assert_list(par_vals, names = "unique", any.missing = FALSE)
  assert_subset(names(par_vals), par_set$ids)
  par_vals
}

assert_range = function(range) {
  assert_numeric(range, len = 2L, any.missing = FALSE)
  if (diff(range) <= 0)
    stopf("Invalid range specified. First value (%f) must be greater than second value (%f)", range[1L], range[2L])
  range
}

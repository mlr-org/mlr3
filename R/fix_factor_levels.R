fix_factor_levels = function(data, levels, ...) {
  UseMethod("fix_factor_levels")
}

#' @export
fix_factor_levels.data.table = function(data, levels, ...) { # nolint
  levels = levels[intersect(names(levels), names(data))]
  iwalk(levels, function(lvls, id, data) {
    x = data[[id]]
    if (!identical(levels(x), lvls)) {
      set(data, j = id, value = factor(x, levels = lvls, ordered = is.ordered(x)))
    }
  }, data = data)
  data[]
}

#' @export
fix_factor_levels.Matrix = function(data, levels, ...) { # nolint
  levels = levels[intersect(names(levels), names(data))]
  if (length(levels)) {
    stop("Factor handling in Matrix data is not supported")
  }
  data
}

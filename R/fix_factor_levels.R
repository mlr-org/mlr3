fix_factor_levels = function(data, levels, ...) {
  UseMethod("fix_factor_levels")
}

#' @export
fix_factor_levels.data.table = function(data, levels, ...) { # nolint
  levels = levels[intersect(names(levels), names(data))]
  imap(levels, function(lvls, id) {
    x = data[[id]]
    if (!identical(levels(x), lvls)) {
      set(data, j = id, value = factor(x, levels = lvls, ordered = is.ordered(x)))
    }
  })
  data[]
}

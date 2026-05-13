fix_factor_levels = function(data, levels, ...) {
  UseMethod("fix_factor_levels")
}

#' @export
# nolint next
fix_factor_levels.data.table = function(data, levels, ...) {
  levels = levels[intersect(names(levels), names(data))]
  iwalk(
    levels,
    function(lvls, id, data) {
      x = data[[id]]
      if (!identical(levels(x), lvls)) {
        set(data, j = id, value = factor(x, levels = lvls, ordered = is.ordered(x)))
      }
    },
    data = data
  )
  data[]
}

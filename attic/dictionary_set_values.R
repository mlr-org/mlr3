
      set_values = function(x, ...) {
        if (...length()) {
          dots = list(...)
          nn = names(dots)
          for (i in seq_along(dots)) {
            x[[nn[i]]] = dots[[i]]
          }
        }
        x
      }

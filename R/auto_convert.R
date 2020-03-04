auto_convert = function(value, id, type, levels) {
  UseMethod("auto_convert", value)
}

#' @export
auto_convert.default  = function(value, id, type, levels) {
  print(str(value))
}

#' @export
auto_convert.logical = function(value, id, type, levels) {
  switch(type,
    logical = {
      return(value)
    },
    integer = {
      return(as.integer(value))
    },
    numeric = {
      return(as.double(value))
    },
    character = {
      return(as.character(value))
    },
    factor = {
      return(as_factor(value, levels = union(levels, value), ordered = FALSE))
    },
    ordered = {
      if (!all(value %in% c(levels, NA)))
        warningf("Detected data loss during conversion of column '%s' to ordered", id)
      return(as_factor(value, levels = levels, ordered = TRUE))
    }
  )

  stopf("Cannot convert column '%s' to '%s' Incompatible type '%s'.", id, type, class(value)[1L])
}

#' @export
auto_convert.integer = function(value, id, type, levels) {
  switch(type,
    logical = {
      if (!test_integerish(value, lower = 0L, upper = 1L))
        warningf("Detected data loss during conversion of column '%s' to logical", id)
      return(as.logical(value))
    },
    integer = {
      return(value)
    },
    numeric = {
      return(as.double(value))
    },
    character = {
      return(as.character(value))
    },
    factor = {
      return(as_factor(value, levels = union(levels, value), ordered = FALSE))
    },
    ordered = {
      if (!all(value %in% c(levels, NA)))
        warningf("Detected data loss during conversion of column '%s' to ordered", id)
      return(as_factor(value, levels = levels, ordered = TRUE))
    }
  )
  stopf("Cannot convert column '%s' to '%s' Incompatible type '%s'.", id, type, class(value)[1L])
}


#' @export
auto_convert.numeric = function(value, id, type, levels) {
  switch(type,
    logical = {
      if (!test_integerish(value, lower = 0L, upper = 1L))
        warningf("Detected data loss during conversion of column '%s' to logical", id)
      return(as.logical(value))
    },
    integer = {
      if (!test_integerish(value))
        warningf("Detected data loss during conversion of column '%s' to integer", id)
      return(as.integer(value))
    },
    numeric = {
      return(value)
    },
    character = {
      return(as.character(value))
    },
    factor = {
      return(as_factor(value, levels = union(levels, value), ordered = FALSE))
    },
    ordered = {
      if (!all(value %in% c(levels, NA)))
        warningf("Detected data loss during conversion of column '%s' to ordered", id)
      return(as_factor(value, levels = levels, ordered = TRUE))
    }
  )
  stopf("Cannot convert column '%s' to '%s' Incompatible type '%s'.", id, type, class(value)[1L])
}


#' @export
auto_convert.character = function(value, id, type, levels) {
  switch(type,
    logical = {
      if (all(is.na(value) | tolower(value) %in% c("true", "false", "t", "f")))
        return(as.logical(value))
    },
    integer = {
      if (allMissing(value))
        return(as.integer(value))
    },
    numeric = {
      if (allMissing(value))
        return(as.double(value))
    },
    character = {
      return(value)
    },
    factor = {
      return(as_factor(value, levels = union(levels, value), ordered = FALSE))
    },
    ordered = {
      if (!all(value %in% c(levels, NA)))
        warningf("Detected data loss during conversion of column '%s' to ordered", id)
      return(as_factor(value, levels = levels, ordered = TRUE))
    }
  )
  stopf("Cannot convert column '%s' to '%s' Incompatible type '%s'.", id, type, class(value)[1L])
}

#' @export
auto_convert.factor = function(value, id, type, levels) {
  switch(type,
    logical = {
      if (allMissing(value))
        return(as.logical(value))
    },
    integer = {
      if (allMissing(value))
        return(as.integer(value))
    },
    numeric = {
      if (allMissing(value))
        return(as.double(value))
    },
    character = {
      return(as.character(value))
    },
    factor = {
      return(as_factor(value, levels = union(levels, levels(value)), ordered = FALSE))
    },
    ordered = {
      if (!all(levels(value) %in% levels))
        warningf("Detected data loss during conversion of column '%s' to ordered", id)
      return(as_factor(value, levels = levels, ordered = TRUE))
    }
  )
  stopf("Cannot convert column '%s' to '%s' Incompatible type '%s'.", id, type, class(value)[1L])
}

#' @export
auto_convert.ordered = function(value, id, type, levels) {
  switch(type,
    logical = {
      if (allMissing(value))
        return(as.logical(value))
    },
    integer = {
      if (allMissing(value))
        return(as.integer(value))
    },
    numeric = {
      if (allMissing(value))
        return(as.double(value))
    },
    character = {
      return(as.character(value))
    },
    factor = {
      return(as_factor(value, levels = union(levels, levels(value)), ordered = FALSE))
    },
    ordered = {
      if (!all(levels(value) %in% levels))
        warningf("Detected data loss during conversion of column '%s' to ordered", id)
      return(as_factor(value, levels = levels, ordered = TRUE))
    }
  )
  stopf("Cannot convert column '%s' to '%s' Incompatible type '%s'.", id, type, class(value)[1L])
}

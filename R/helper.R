#' @title Get 'private' part of an R6 Instance
#'
#' @param x ([R6::R6Class]).
#'
#' @return (`environment()`).
#' @noRd
get_private = function(x) {
  x[[".__enclos_env__"]][["private"]]
}

hashes = function(x) {
  map_chr(unname(x), "hash")
}

phashes = function(x) {
  map_chr(unname(x), "phash")
}

hash = function(...) {
  dots = list(...)
  dots = map_if(dots, is.function, function(fun) {
    list(formals(fun), as.character(body(fun)))
  })
  dots = map_if(dots, is.data.table, as.list)
  digest::digest(dots, algo = "xxhash64")
}

# updating join:
# replaces values in x with values in y
ujoin = function(x, y, key) {
  cn = setdiff(intersect(names(x), names(y)), key)
  expr = parse(text = paste0("`:=`(", paste0(sprintf("%1$s=i.%1$s", cn), collapse = ","), ")"))
  x[y, eval(expr), on = key][]
}

translate_types = function(x) {
  r_types = mlr_reflections$task_feature_types
  p_types = names(mlr_reflections$task_feature_types)
  factor(map_values(x, r_types, p_types), levels = p_types)
}

allow_partial_matching = list(
  warnPartialMatchArgs = FALSE,
  warnPartialMatchAttr = FALSE,
  warnPartialMatchDollar = FALSE
)


# extract values from a single column of a data table
# tries to avoid the overhead of data.table for small tables
fget = function(tab, i, j, key = key(tab)) {
  if (nrow(tab) > 1000L) {
    tab[list(i), j, on = key, with = FALSE][[1L]]
  } else {
    table = tab[[key]]
    if (is.character(table)) {
      tab[[j]][chmatch(i, table, nomatch = 0L)]
    } else {
      tab[[j]][match(i, table, nomatch = 0L)]
    }
  }
}

get_progressor = function(n, label = NA_character_) {
  if (!isNamespaceLoaded("progressr")) {
    return(NULL)
  }

  progressr::progressor(steps = n, label = label)
}

allow_utf8_names = function() {
  isTRUE(getOption("mlr3.allow_utf8_names"))
}


reorder_vector = function(x, y, na_last = NA) {
  order(match(x, y), na.last = na_last)
}

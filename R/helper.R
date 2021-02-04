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


replace_with = function(x, needle, replacement) {
  ii = (x == needle)
  x = rep(x, 1L + (length(replacement) - 1L) * ii)
  replace(x, ii, replacement)
}

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

# the following is a necessary workaround for the new R misbehaving when comparing R6 objects.
# See https://github.com/r-lib/R6/issues/208
# This is quite sloppy right now.
r6_to_list = function(x) {
  actives = c(".__enclos_env__", names(x[[".__enclos_env__"]][[".__active__"]]))
  ll = sapply(setdiff(names(x), actives), get, x, simplify = FALSE)
  ll[[".__enclos_env__"]] = list(`.__active__` = x[[".__enclos_env__"]][[".__active__"]], private = x[[".__enclos_env__"]][["private"]])
  if (!is.null(x[[".__enclos_env__"]][["super"]])) {
    ll[[".__enclos_env__"]][["super"]] = r6_to_list(x[[".__enclos_env__"]][["super"]])
  }
  ln = names(ll)
  attributes(ll) = attributes(x)
  names(ll) = ln
  ll[sort(names(ll))]
}

all.equal.R6 = function(target, current, ...) {
  if (!is.environment(target)) NextMethod()
  if (!is.environment(current)) NextMethod()
  if (!inherits(current, "R6")) return("'current' is not an R6 class")
  # avoid cycles
  r6_seen = dynGet("__r6_seen__", NULL)
  if (is.null(r6_seen)) {
    r6_seen = "__r6_seen__" = new.env(parent = emptyenv())
  }
  tca = sprintf("%s__%s", data.table::address(target), data.table::address(current))
  if (!is.null(r6_seen[[tca]])) return(TRUE)
  r6_seen[[tca]] = TRUE
  # call all.equal.list directly because objects still have R6 class
  base:::all.equal.list(r6_to_list(target), r6_to_list(current),  ...)
}

registerS3method("all.equal", "R6", all.equal.R6)
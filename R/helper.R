hashes = function(x) {
  map_chr(unname(x), "hash")
}

hash = function(...) {
  digest::digest(list(...), algo = "xxhash64")
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

get_progressor = function(n, label = NA_character_) {
  if (!isNamespaceLoaded("progressr")) {
    return(NULL)
  }

  progressr::progressor(steps = n, label = label)
}


replace_with = function(x, needle, replacement) {
  ii = (x == needle)
  x = rep(x, 1L + (length(replacement) - 1L) * ii)
  replace(x, ii, replacement)
}


# determines if execution via future will be running locally or remotely
use_future = function() {
  isNamespaceLoaded("future") && !inherits(future::plan(), "uniprocess")
}

get_rng_state = function() {
  list(seed = get_seed(), kind = RNGkind())
}

restore_rng_state = function(prev) {
  do.call(RNGkind, as.list(prev$kind))
  assign(".Random.seed", value = prev$seed, envir = .GlobalEnv)
}

init_future_seeding = function(n) {
  RNGkind("L'Ecuyer-CMRG")
  getFromNamespace("make_rng_seeds", asNamespace("future.apply"))(n, TRUE)
}

# TODO: remove after mlr3misc update
rd_format_packages = function(pkgs) {
  if (length(pkgs) == 0L)

    return("-")
  base_pkgs = c("base", "compiler", "datasets", "graphics", "grDevices", "grid", "methods",
    "parallel", "splines", "stats", "stats4", "tcltk", "tools", "translations", "utils"
  )
  link = pkgs %nin% base_pkgs
  str_collapse(sprintf("%s%s%s",
    ifelse(link, "\\CRANpkg{", "'"),
    pkgs,
    ifelse(link, "}", "'")
  ))
}

#' Replace R6 Objects with their Hash
#'
#' @description
#' Given a table `tab` with list column `col` of R6 objects, walks over values of `col`
#' and replaces R6 objects with the string of their respective hash.
#' The replaced objects are returned as a named list.
#'
#' This operation can be reversed with the [replace_with_object()] function.
#'
#' @param tab (`data.table()`).
#' @param col (`character(1)`)\cr
#'   Column of `tab`.
#'
#' @return (named `list()`).
#' List of distinct extracted R6 objects, named with their hash.
#'
#' @noRd
replace_with_hash = function(tab, col) {
  values = tab[[col]]
  hashes = hashes(values)
  uniq = !duplicated(hashes)
  objs = set_names(values[uniq], hashes[uniq])
  set(tab, j = col, value = hashes)
  objs
}

#' Replace Hashes with their R6 Objects
#'
#' @description
#' Given a table `tab` with column `col` of R6 hashes and a named list `objects` of R6 objects,
#' replaces the hashes with the corresponding R6 object.
#'
#' This operation reverses the [replace_with_hash()] function.
#'
#' @param tab (`data.table()`).
#' @param col (`character(1)`)\cr
#'   Column of `tab`.
#' @param objs (named `list()`)\cr
#'   List as returned by [replace_with_hash()].
#'
#' @return (`data.table()`).
#' Hashes are replaced by the corresponding R6 objects.
#'
#' @noRd
replace_with_object = function(tab, col, objs) {
  set(tab, j = col, value = objs[tab[[col]]])
}

reassemble_learner = function(learner, state) {
  Map(function(l, s) {
        l$clone()
        l$state = s
        l
  }, l = learner, s = state)
}

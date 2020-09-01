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
  state = list(seed = get_seed(), kind = RNGkind())
  names(state$kind) = c("kind", "normal.kind", "sample.kind")
  state
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

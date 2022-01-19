#' @title Install (Missing) Packages
#'
#' @description
#' `extract_pkgs()` extracts required package from various objects, including
#' [TaskGenerator], [Learner], [Measure] and objects from
#' extension packages such as \CRANpkg{mlr3pipelines} or \CRANpkg{mlr3filters}.
#' If applied on a list, the function is called recursively on all elements.
#'
#' `install_pkgs()` calls `extract_pkgs()` internally and proceeds with the
#' installation of extracted packages.
#'
#' @details
#' If a package contains a forward slash ('/'), it is assumed to be a package hosted
#' on GitHub in `"<user>/<repo>"` format, and the string will be passed to
#' [remotes::install_github()].
#' Otherwise, the package name will be passed to [remotes::install_cran()].
#'
#' @param x (any)\cr
#'   Object with package information (or a list of such objects).
#' @param ... (any)\cr
#'   Additional arguments passed down to [remotes::install_cran()] or
#'   [remotes::install_github()].
#'   Arguments `force` and `upgrade` are often important in this context.
#'
#' @return `extract_pkgs()` returns a `character()` of package strings,
#'   `install_pkgs()` returns the names of extracted packages invisibly.
#'
#' @export
#' @examples
#' extract_pkgs(lrns(c("regr.rpart", "regr.featureless")))
install_pkgs = function(x, ...) {
  require_namespaces("remotes")
  pkg = NULL

  # build initial table
  tab = data.table(pkg = extract_pkgs(x))
  set(tab, j = "name", value = map_chr(strsplit(tab$pkg, "/", fixed = TRUE), tail, 1L))

  # filter duplicates, keep github versions
  set(tab, j = "github", value = grepl("/", tab$pkg, fixed = TRUE))
  setkeyv(tab, c("github", "name"))
  tab = unique(tab, by = "name", fromLast = TRUE)

  # never update self
  tab = tab[get("name") != "mlr3"]

  # install cran packages first
  pkgs = tab[list(FALSE), pkg, on = "github", nomatch = NULL]
  remotes::install_cran(pkgs, ...)

  # install github packages
  pkgs = tab[list(TRUE), pkg, on = "github", nomatch = NULL]
  remotes::install_github(pkgs, ...)

  # return names of extracted packages
  invisible(tab$name)
}

#' @rdname install_pkgs
#' @export
extract_pkgs = function(x) {
  UseMethod("extract_pkgs")
}

#' @rdname install_pkgs
#' @export
extract_pkgs.character = function(x) {
  x
}

#' @rdname install_pkgs
#' @export
extract_pkgs.R6 = function(x) { # nolint
  get0("packages", envir = x, inherits = FALSE, ifnotfound = character())
}

#' @rdname install_pkgs
#' @export
extract_pkgs.list = function(x) { # nolint
  unique(unlist(lapply(x, extract_pkgs), recursive = FALSE, use.names = FALSE))
}

#' @rdname install_pkgs
#' @export
extract_pkgs.ResampleResult = function(x) { # nolint
  extract_pkgs(x$learner)
}

#' @rdname install_pkgs
#' @export
extract_pkgs.BenchmarkResult = function(x) { # nolint
  extract_pkgs(x$learners$learner)
}

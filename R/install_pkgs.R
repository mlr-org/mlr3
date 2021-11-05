#' @title Install (missing) packages
#'
#' @description
#' Install required packages.
#' Extracts the package information from various objects, including
#' [TaskGenerator], [Learner], [Measure] and objects from
#' extension packages such as \CRANpkg{mlr3pipelines} or \CRANpkg{mlr3filters}.
#'
#' If provided a list, the function is called recursively on all elements.
#'
#' @param x (any)\cr
#'   Object with package information (or a list of such objects).
#' @param update (`logical(1)`)\cr
#'   Update packages which are already installed?
#' @param force (`logical(1)`)\cr
#'   Force installation, even if the package is already installed.
#' @export
install_pkgs = function(x, update = TRUE, force = FALSE) { # nolint
  assert_flag(update)
  assert_flag(force)
  require_namespaces("remotes")

  # build initial table
  tab = data.table(pkg = extract_pkgs(x))
  set(tab, j = "name", value = map_chr(strsplit(tab$pkg, "/", fixed = TRUE), tail, 1L))

  # filter duplicates, keep github versions
  set(tab, j = "github", value = grepl("/", tab$pkg, fixed = TRUE))
  setorderv(tab, "github")
  tab = unique(tab, by = "name", fromLast = TRUE)

  # remove installed packages if no update is required
  if (!update) {
    is_pkg_installed = function(pkg) {
      length(find.package(pkg, quiet = TRUE)) > 0L
    }

    keep = !map_lgl(tab$name, is_pkg_installed)
    tab = tab[keep]
  }

  pkgs = tab[get("github") == FALSE]$pkg
  remotes::install_cran(pkgs, force = force)

  pkgs = tab[get("github") == TRUE]$pkg
  remotes::install_github(pkgs, force = force)

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

#' @export
extract_pkgs.R6 = function(x) { # nolint
  get0("packages", envir = x, inherits = FALSE, ifnotfound = character())
}

#' @export
extract_pkgs.list = function(x) { # nolint
  unique(unlist(lapply(x, extract_pkgs), recursive = FALSE, use.names = FALSE))
}

#' @export
extract_pkgs.ResampleResult = function(x) { # nolint
  extract_pkgs(x$learner)
}

#' @export
extract_pkgs.BenchmarkResult = function(x) { # nolint
  extract_pkgs(x$learners$learner)
}

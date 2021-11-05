#' @title Install (missing) packages
#' @export
install_pkgs = function(x, ...) { # nolint
  UseMethod("install_pkgs")

}

install_pkgs.Learner = function(x, ...) {
  install_pkg(x)
}

install_pkgs.Pipeop = function(x, ...) {
  install_pkg(x)
}

install_pkgs.Graph = function(x, ...) {
  install_pkg(x)
}

install_pkgs.list = function(x, ...) {
  lapply(x, install_pkgs)
}

install_pkg = function(x, force = FALSE) {
  if (!force && length(find.package(x))) {
    return(TRUE)
  }

  if (grepl("/", x, fixed = TRUE)
    remotes::install_github(x)
  else
    install.packages(x)
}

mlrmisc::install_pkgs(x)


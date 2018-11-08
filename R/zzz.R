#' @import checkmate
#' @import data.table
#' @import paradox
#' @importFrom R6 R6Class
#' @importFrom utils data head tail adist
#' @importFrom stats setNames reformulate median mad runif
#'
#' @section Options:
#' Available package options are documented in [mlr_options()].
#'
"_PACKAGE"

.onLoad = function(libname, pkgname) { #nocov start
  utils::globalVariables(c("id", "role"), package = "mlr3")

  backports::import(pkgname)

  # Set default options without overwriting already set options
  opts = default_opts[match(names(default_opts), names(.Options), nomatch = 0L) == 0L]
  if (length(opts))
    options(opts)
} #nocov end

.onAttach = function(libname, pkgname) { #nocov start
  packageStartupMessage("The mlr3 package is currently work-in-progress. Do not use in production. The API will change. You have been warned.")
} #nocov end

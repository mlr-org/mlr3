#' @import checkmate
#' @import data.table
#' @import paradox
#' @import mlr3misc
#' @importFrom purrr map map_lgl map_int map_dbl map_chr set_names
#' @importFrom R6 R6Class
#' @importFrom utils data head tail adist
#' @importFrom stats reformulate median mad runif
#'
#' @section Options:
#' Available package options are documented in [mlr_options()].
#'
"_PACKAGE"

.onLoad = function(libname, pkgname) { #nocov start
  utils::globalVariables(c("id", "role"), package = "mlr3")

  backports::import(pkgname)

  # Set default options without overwriting already set options
  opts = mlr_reflections$default_mlr_options
  opts = opts[match(names(opts), names(.Options), nomatch = 0L) == 0L]
  if (length(opts))
    options(opts)
} #nocov end

.onAttach = function(libname, pkgname) { #nocov start
  packageStartupMessage("The mlr3 package is currently work-in-progress. Do not use in production. The API will change. You have been warned.")
} #nocov end

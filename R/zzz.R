#' @rawNamespace import(data.table, except = transpose)
#' @import checkmate
#' @import paradox
#' @import mlr3misc
#' @importFrom R6 R6Class is.R6
#' @importFrom utils data head tail
#' @importFrom stats reformulate median mad runif rnorm
"_PACKAGE"

dummy_import = function() { # nocov start
  # this function is required to silence R CMD check
  tmp = Metrics::ce
} # nocov end


.onLoad = function(libname, pkgname) { #nocov start
  backports::import(pkgname)
  assign("lg", lgr::get_logger(pkgname), envir = parent.env(environment()))
} #nocov end

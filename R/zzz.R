#' @rawNamespace import(data.table, except = transpose)
#' @import checkmate
#' @import paradox
#' @import mlr3misc
#' @importFrom logger DEBUG log_debug INFO log_info WARN log_warn ERROR log_error with_log_threshold
#' @importFrom R6 R6Class
#' @importFrom utils data head tail adist
#' @importFrom stats reformulate median mad runif
#'
"_PACKAGE"

layout_mlr3 <- structure(function(level, msg) {
  paste0(attr(level, 'level'), ' [mlr3] ', msg)
}, generator = quote(layout_mlr3()))

.onLoad = function(libname, pkgname) { #nocov start
  backports::import(pkgname)
  logger::log_formatter(logger::formatter_sprintf, namespace = pkgname)
  logger::log_layout(layout_mlr3, namespace = pkgname)

  # utils::globalVariables(c("id", "role"), package = "mlr3")
} #nocov end

.onAttach = function(libname, pkgname) { #nocov start
  packageStartupMessage("The mlr3 package is currently work-in-progress. Do not use in production. The API will change. You have been warned.")
} #nocov end

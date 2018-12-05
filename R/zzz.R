#' @import checkmate
#' @import data.table
#' @import paradox
#' @import mlr3misc
#' @import logger
#' @importFrom R6 R6Class
#' @importFrom utils data head tail adist
#' @importFrom stats reformulate median mad runif
#'
"_PACKAGE"

.onLoad = function(libname, pkgname) { #nocov start
  backports::import(pkgname)
  logger::log_formatter(logger::formatter_sprintf, namespace = pkgname)
  logger::log_threshold(INFO, namespace = pkgname)
  # utils::globalVariables(c("id", "role"), package = "mlr3")
} #nocov end

.onAttach = function(libname, pkgname) { #nocov start
  packageStartupMessage("The mlr3 package is currently work-in-progress. Do not use in production. The API will change. You have been warned.")
} #nocov end

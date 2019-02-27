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
  tmp = bit::bit
  tmp = Metrics::ce
} # nocov end


.onLoad = function(libname, pkgname) { #nocov start
  backports::import(pkgname)
  log = lgr::Logger$new(name = "mlr3", appenders = list(console = lgr::AppenderConsole$new()), propagate = FALSE)
  log$appenders$console$set_layout(lgr::LayoutFormat$new(fmt = "[%L] %m"))

  assign("log", log, envir = parent.env(environment())
  )
  # utils::globalVariables(c("id", "role"), package = "mlr3")
} #nocov end

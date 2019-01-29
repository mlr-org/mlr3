#' @rawNamespace import(data.table, except = transpose)
#' @import checkmate
#' @import paradox
#' @import mlr3misc
#' @importFrom logger DEBUG log_debug INFO log_info WARN log_warn ERROR log_error with_log_threshold
#' @importFrom withr with_seed
#' @importFrom R6 R6Class is.R6
#' @importFrom utils data head tail adist
#' @importFrom stats reformulate median mad runif rnorm
"_PACKAGE"

dummy_import = function() { # nocov start
  # this function is required to silence R CMD check
  tmp = bit::bit
  tmp = Metrics::ce
} # nocov end

layout_mlr3 = structure(
  function(level, msg, namespace = NA_character_, .logcall = sys.call(), .topcall = sys.call(-1), .topenv = parent.frame()) {
    paste0(attr(level, 'level'), ' [mlr3] ', msg)
  }, generator = quote(layout_mlr3())
)

.onLoad = function(libname, pkgname) { #nocov start
  backports::import(pkgname)
  logger::log_formatter(logger::formatter_sprintf, namespace = pkgname)
  logger::log_layout(layout_mlr3, namespace = pkgname)
  # utils::globalVariables(c("id", "role"), package = "mlr3")
} #nocov end

#' @import data.table
#' @import checkmate
#' @import paradox
#' @import mlr3misc
#' @importFrom R6 R6Class is.R6
#' @importFrom utils data head
#' @importFrom uuid UUIDgenerate
#' @section Additional resources:
#' * Book on mlr3: \url{https://mlr3book.mlr-org.com}
#' * Use cases and examples: \url{https://mlr3gallery.mlr-org.com}
#' @references
#' \cite{mlr3}{pkg::citation}
"_PACKAGE"

dummy_import = function() {
  # nocov start
  # this function is required to silence R CMD check
  mlbench::mlbench.xor
  mlr3measures::mse
} # nocov end


.onLoad = function(libname, pkgname) {
  # nocov start
  backports::import(pkgname)

  # setup logger
  assign("lg", lgr::get_logger(pkgname), envir = parent.env(environment()))
  if (Sys.getenv("IN_PKGDOWN") == "true") {
    lg$set_threshold("warn")
  }
} # nocov end

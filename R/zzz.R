#' @import data.table
#' @import checkmate
#' @import paradox
#' @import mlr3misc
#' @importFrom R6 R6Class is.R6
#' @importFrom utils data head tail getFromNamespace
#' @importFrom graphics plot
#' @importFrom stats predict rnorm runif sd contr.treatment
#' @importFrom uuid UUIDgenerate
#' @section Additional resources:
#' * Book on mlr3: \url{https://mlr3book.mlr-org.com}
#' * Use cases and examples: \url{https://mlr3gallery.mlr-org.com}
#' * More classification and regression learners: \CRANpkg{mlr3learners}
#' * Preprocessing and machine learning pipelines: \CRANpkg{mlr3pipelines}
#' * Tuning of hyperparameters: \CRANpkg{mlr3tuning}
#' * Visualizations for many \pkg{mlr3} objects: \CRANpkg{mlr3viz}
#' * Survival analysis and probabilistic regression: \CRANpkg{mlr3proba}
#' * Feature selection filters: \CRANpkg{mlr3filters}
#' * Interface to real (out-of-memory) data bases: \CRANpkg{mlr3db}
#' * Performance measures as plain functions: \CRANpkg{mlr3measures}
#' * Parallelization framework: \CRANpkg{future}
#' * Progress bars: \CRANpkg{progressr}
#' @references
#' `r tools::toRd(citation("mlr3"))`
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

leanify_package()

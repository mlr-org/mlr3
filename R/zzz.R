#' @import checkmate
#' @import data.table
#' @import paradox
#' @importFrom R6 R6Class
#' @importFrom utils data head tail adist
#' @importFrom stats setNames reformulate median mad
NULL

populate_dicts = function() {
  mlr_learners$add(LearnerClassifCrashtest$new())
  mlr_learners$add(LearnerClassifDummy$new())
  mlr_learners$add(LearnerRegrDummy$new())
  mlr_learners$add(LearnerClassifRpart$new())
  mlr_learners$add(LearnerRegrRpart$new())

  mlr_resamplings$add(ResamplingBootstrap$new())
  mlr_resamplings$add(ResamplingCV$new())
  mlr_resamplings$add(ResamplingSubsampling$new())
  mlr_resamplings$add(ResamplingHoldout$new())
  mlr_resamplings$add(ResamplingRepeatedCV$new())
  mlr_resamplings$add(ResamplingCustom$new())

  mlr_measures$add(MeasureMMCE$new())
  mlr_measures$add(MeasureACC$new())
  mlr_measures$add(MeasureMSE$new())
}

.onLoad = function(libname, pkgname) { #nocov start
  utils::globalVariables(c("id", "role"), package = "mlr3")

  backports::import(pkgname)
  backports::import(pkgname, "hasName", force = TRUE)

  # Set default options without overwriting already set options
  opts = default_opts[match(names(default_opts), names(.Options), nomatch = 0L) == 0L]
  if (length(opts))
    options(opts)

  populate_dicts()
} #nocov end

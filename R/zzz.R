#' @import checkmate
#' @import data.table
#' @import paradox
#' @importFrom R6 R6Class
#' @importFrom future futureCall value
#' @importFrom future.apply future_lapply future_mapply
#' @importFrom utils data head tail adist
NULL

# environment which holds constants and allows for reflections
mlr3 = new.env(parent = emptyenv())

.onLoad = function(libname, pkgname) { #nocov start
  utils::globalVariables(c("role"), package = "mlr3")

  backports::import(pkgname)
  backports::import(pkgname, "hasName", force = TRUE)


  opts = list(
    mlr3.verbose = TRUE,
    mlr3.debug = FALSE
  )
  # Set options, but do not overwrite user settings
  opts = opts[match(names(opts), names(.Options), nomatch = 0L) == 0L]
  if (length(opts))
    options(opts)


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
  mlr_measures$add(MeasureMSE$new())
} #nocov end

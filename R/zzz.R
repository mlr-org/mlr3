#' @import checkmate
#' @import data.table
#' @import paradox
#' @importFrom R6 R6Class
#' @importFrom stats setNames predict
#' @importFrom utils data head tail
NULL

# environment which holds constants and allows for reflections
mlr3 = new.env(parent = emptyenv())

mlr3$default.opts = list(
  mlr3.verbose = TRUE,
  mlr3.debug = FALSE
)

.onLoad = function(libname, pkgname) { #nocov start
  utils::globalVariables(c("role"), package = "mlr3")

  backports::import(pkgname)
  backports::import(pkgname, "hasName", force = TRUE)

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

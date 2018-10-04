#' @import checkmate
#' @import data.table
#' @import paradox
#' @importFrom R6 R6Class
#' @importFrom utils data head tail adist
#' @importFrom stats setNames reformulate median mad
"_PACKAGE"

populate_dicts = function() {
  # FIXME: automate this by creating a simple register?

  if (is.null(mlr_tasks))
     mlr_tasks <<- DictionaryTask$new()
  for (lv in lazy_tasks) mlr_tasks$add(lv)

  if (is.null(mlr_learners))
    mlr_learners <<- DictionaryLearner$new()
  mlr_learners$add(LearnerClassifCrashtest$new())
  mlr_learners$add(LearnerClassifDummy$new())
  mlr_learners$add(LearnerRegrDummy$new())
  mlr_learners$add(LearnerClassifRpart$new())
  mlr_learners$add(LearnerRegrRpart$new())

  if (is.null(mlr_resamplings))
    mlr_resamplings <<- DictionaryResampling$new()
  mlr_resamplings$add(ResamplingBootstrap$new())
  mlr_resamplings$add(ResamplingCV$new())
  mlr_resamplings$add(ResamplingSubsampling$new())
  mlr_resamplings$add(ResamplingHoldout$new())
  mlr_resamplings$add(ResamplingRepeatedCV$new())
  mlr_resamplings$add(ResamplingCustom$new())

  if (is.null(mlr_measures))
    mlr_measures <<- DictionaryMeasure$new()
  mlr_measures$add(MeasureElapsedTime$new("time_train", "train"))
  mlr_measures$add(MeasureElapsedTime$new("time_predict", "predict"))
  mlr_measures$add(MeasureElapsedTime$new("time_both", c("train", "predict")))
  mlr_measures$add(MeasureClassifMMCE$new())
  mlr_measures$add(MeasureClassifACC$new())
  mlr_measures$add(MeasureClassifAUC$new())
  mlr_measures$add(MeasureRegrMSE$new())
}

.onLoad = function(libname, pkgname) { #nocov start
  utils::globalVariables(c("id", "role"), package = "mlr3")

  backports::import(pkgname)

  # Set default options without overwriting already set options
  opts = default_opts[match(names(default_opts), names(.Options), nomatch = 0L) == 0L]
  if (length(opts))
    options(opts)

  populate_dicts()
} #nocov end

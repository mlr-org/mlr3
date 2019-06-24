#' @import data.table
#' @import checkmate
#' @import paradox
#' @import mlr3misc
#' @importFrom R6 R6Class is.R6
#' @importFrom utils data head tail
#' @importFrom stats reformulate
"_PACKAGE"

dummy_import = function() {
  # nocov start
  # this function is required to silence R CMD check
  tmp = Metrics::ce
  tmp = mlbench::mlbench.xor
} # nocov end


populate_dictionaries = function() {

  # Populate Tasks
  if (is.null(mlr_tasks))
    mlr_tasks <<- DictionaryTask$new()
  mlr_tasks$add("boston_housing", load_task_boston_housing)
  mlr_tasks$add("iris", load_task_iris)
  mlr_tasks$add("german_credit", load_task_german_credit)
  mlr_tasks$add("mtcars", load_task_mtcars)
  mlr_tasks$add("pima", load_task_pima)
  mlr_tasks$add("sonar", load_task_sonar)
  mlr_tasks$add("spam", load_task_spam)
  mlr_tasks$add("wine", load_task_wine)
  mlr_tasks$add("zoo", load_task_zoo)


  # Populate Generators
  if (is.null(mlr_generators))
    mlr_generators <<- DictionaryGenerator$new()
  mlr_generators$add("2dnormals", Generator2DNormals)
  mlr_generators$add("friedman1", GeneratorFriedman1)
  mlr_generators$add("smiley", GeneratorSmiley)
  mlr_generators$add("xor", GeneratorXor)


  # Populate Learners
  if (is.null(mlr_learners))
    mlr_learners <<- DictionaryLearner$new()
  mlr_learners$add("classif.debug", LearnerClassifDebug)
  mlr_learners$add("classif.featureless", LearnerClassifFeatureless)
  mlr_learners$add("classif.rpart", LearnerClassifRpart)
  mlr_learners$add("regr.featureless", LearnerRegrFeatureless)
  mlr_learners$add("regr.rpart", LearnerRegrRpart)


  # Populate Measures
  if (is.null(mlr_measures))
    mlr_measures <<- DictionaryMeasure$new()
  mlr_measures$add("classif.acc", MeasureClassifACC)
  mlr_measures$add("classif.auc", MeasureClassifAUC)
  mlr_measures$add("classif.costs", MeasureClassifCosts)
  mlr_measures$add("classif.f1", MeasureClassifF1)
  mlr_measures$add("classif.ce", MeasureClassifCE)
  mlr_measures$add("oob_error", MeasureOOBError)
  mlr_measures$add("regr.mae", MeasureRegrMAE)
  mlr_measures$add("regr.mse", MeasureRegrMSE)
  mlr_measures$add("selected_features", MeasureSelectedFeatures)
  mlr_measures$add("time_train", MeasureElapsedTime, id = "time_train", parts = "train")
  mlr_measures$add("time_predict", MeasureElapsedTime, id = "time_predict", parts = "predict")
  mlr_measures$add("time_both", MeasureElapsedTime, id = "time_both", parts = c("train", "predict"))
  for (type in confusion_measure_info$id) {
    id = sprintf("classif.%s", type)
    mlr_measures$add(id, MeasureClassifConfusion, id = id, type = type)
  }

  # Populate Resamplings
  if (is.null(mlr_resamplings))
    mlr_resamplings <<- DictionaryResampling$new()
  mlr_resamplings$add("bootstrap", ResamplingBootstrap)
  mlr_resamplings$add("custom", ResamplingCustom)
  mlr_resamplings$add("cv", ResamplingCV)
  mlr_resamplings$add("cv3", ResamplingCV, id = "cv3", param_vals = list(folds = 3L))
  mlr_resamplings$add("holdout", ResamplingHoldout)
  mlr_resamplings$add("repeated_cv", ResamplingRepeatedCV)
  mlr_resamplings$add("subsampling", ResamplingSubsampling)
}

.onLoad = function(libname, pkgname) {
  # nocov start
  backports::import(pkgname)

  # setup logger
  assign("lg", lgr::get_logger(pkgname), envir = parent.env(environment()))
  if (Sys.getenv("IN_PKGDOWN") == "true") {
    lg$set_threshold("warn")
  }

  populate_dictionaries()
} # nocov end

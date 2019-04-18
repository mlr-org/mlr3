#' @rawNamespace import(data.table, except = transpose)
#' @import checkmate
#' @import paradox
#' @import mlr3misc
#' @importFrom logger DEBUG log_debug INFO log_info WARN log_warn ERROR log_error with_log_threshold
#' @importFrom R6 R6Class is.R6
#' @importFrom utils data head tail
#' @importFrom stats reformulate
"_PACKAGE"

dummy_import = function() { # nocov start
  # this function is required to silence R CMD check
  tmp = Metrics::ce
  tmp = mlbench::mlbench.xor
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

  # Populate Tasks
  mlr_tasks <<- DictionaryTask$new()
  mlr_tasks$add("bh", load_task_bh)
  mlr_tasks$add("iris", load_task_iris)
  mlr_tasks$add("mtcars", load_task_mtcars)
  mlr_tasks$add("pima", load_task_pima)
  mlr_tasks$add("sonar", load_task_sonar)
  mlr_tasks$add("spam", load_task_spam)
  mlr_tasks$add("wine", load_task_wine)
  mlr_tasks$add("zoo", load_task_zoo)


  # Populate Generators
  mlr_generators <<- DictionaryGenerator$new()
  mlr_generators$add("2dnormals", Generator2DNormals)
  mlr_generators$add("smiley", GeneratorSmiley)
  mlr_generators$add("xor", GeneratorXor)


  # Populate Learners
  mlr_learners <<- DictionaryLearner$new()
  mlr_learners$add("classif.debug", LearnerClassifDebug)
  mlr_learners$add("classif.featureless", LearnerClassifFeatureless)
  mlr_learners$add("classif.rpart", LearnerClassifRpart)
  mlr_learners$add("regr.featureless", LearnerRegrFeatureless)
  mlr_learners$add("regr.rpart", LearnerRegrRpart)


  # Populate Measures
  mlr_measures <<- DictionaryMeasure$new()
  mlr_measures$add("classif.acc", MeasureClassifACC)
  mlr_measures$add("classif.auc", MeasureClassifAUC)
  mlr_measures$add("classif.costs", MeasureClassifCosts)
  mlr_measures$add("classif.f1", MeasureClassifF1)
  mlr_measures$add("classif.mmce", MeasureClassifMMCE)
  mlr_measures$add("oob_error", MeasureOOBError)
  mlr_measures$add("regr.mae", MeasureRegrMAE)
  mlr_measures$add("regr.mse", MeasureRegrMSE)
  mlr_measures$add("selected_features", MeasureSelectedFeatures)
  mlr_measures$add("time_both", MeasureTimeBoth)
  mlr_measures$add("time_predict", MeasureTimePredict)
  mlr_measures$add("time_train", MeasureTimeTrain)
  generate_conf_measure = function(type) { force(type); function() MeasureClassifConfusion$new(type) }
  for (type in confusion_measure_info$id)
    mlr_measures$add(sprintf("classif.%s", type), generate_conf_measure(type))

  # Populate Resamplings
  mlr_resamplings <<- DictionaryResampling$new()
  mlr_resamplings$add("bootstrap", ResamplingBootstrap)
  mlr_resamplings$add("custom", ResamplingCustom)
  mlr_resamplings$add("cv", ResamplingCV)
  mlr_resamplings$add("holdout", ResamplingHoldout)
  mlr_resamplings$add("repeated_cv", ResamplingRepeatedCV)
  mlr_resamplings$add("subsampling", ResamplingSubsampling)
} #nocov end

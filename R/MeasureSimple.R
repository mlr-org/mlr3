#' @include MeasureClassif.R
MeasureBinarySimple = R6Class("MeasureBinaryimple",
  inherit = MeasureClassif,
  public = list(
    fun = NULL,
    na_value = NaN,
    initialize = function(name) {
      info = mlr3measures::measures[[name]]
      super$initialize(
        id = paste0("classif.", name),
        range = c(info$lower, info$upper),
        minimize = info$minimize,
        predict_type = info$predict_type,
        task_properties = "twoclass",
        packages = "mlr3measures",
        man = paste0("mlr3measures::", name)
      )
      self$fun = get(name, envir = asNamespace("mlr3measures"), mode = "function")
    },

    score = function(prediction, ...) {
      assert_prediction(prediction)
      measure_score(self, prediction)
    },

    score_internal = function(prediction, ...) {
      truth = prediction$truth
      positive = levels(truth)[1L]
      self$fun(truth = truth, response = prediction$response, prob = prediction$prob[, positive], positive = positive, na_value = self$na_value)
    }
  )
)

#' @include MeasureClassif.R
MeasureClassifSimple = R6Class("MeasureClassifSimple",
  inherit = MeasureClassif,
  public = list(
    fun = NULL,
    na_value = NaN,
    initialize = function(name) {
      info = mlr3measures::measures[[name]]
      super$initialize(
        id = paste0("classif.", name),
        range = c(info$lower, info$upper),
        minimize = info$minimize,
        predict_type = info$predict_type,
        packages = "mlr3measures",
        man = paste0("mlr3measures::", name)
      )
      self$fun = get(name, envir = asNamespace("mlr3measures"), mode = "function")
    },

    score = function(prediction, ...) {
      assert_prediction(prediction)
      measure_score(self, prediction)
    },

    score_internal = function(prediction, ...) {
      self$fun(truth = prediction$truth, response = prediction$response, prob = prediction$prob, na_value = self$na_value)
    }
  )
)

#' @include MeasureRegr.R
MeasureRegrSimple = R6Class("MeasureRegrSimple",
  inherit = MeasureRegr,
  public = list(
    fun = NULL,
    na_value = NaN,
    initialize = function(name) {
      info = mlr3measures::measures[[name]]
      super$initialize(
        id = paste0("regr.", name),
        range = c(info$lower, info$upper),
        minimize = info$minimize,
        predict_type = info$predict_type,
        packages = "mlr3measures",
        man = paste0("mlr3measures::", name)
      )
      self$fun = get(name, envir = asNamespace("mlr3measures"), mode = "function")
    },

    score = function(prediction, ...) {
      assert_prediction(prediction)
      measure_score(self, prediction)
    },

    score_internal = function(prediction, ...) {
      self$fun(truth = prediction$truth, response = prediction$response, se = prediction$se, na_value = self$na_value)
    }
  )
)


#' @title Measures implemented in \CRANpkg{mlr3measures}
#'
#' @name MeasureSimple
#' @usage NULL
#' @include mlr_measures.R
#' @aliases
#'   mlr_measures_classif.tpr
#'   mlr_measures_classif.auc
#'   mlr_measures_regr.mse
#'   mlr_measures_classif.ce
#'   mlr_measures_regr.rse
#'   mlr_measures_classif.tn
#'   mlr_measures_classif.precision
#'   mlr_measures_classif.tp
#'   mlr_measures_classif.specificity
#'   mlr_measures_classif.sensitivity
#'   mlr_measures_classif.fbeta
#'   mlr_measures_classif.tnr
#'   mlr_measures_classif.dor
#'   mlr_measures_classif.recall
#'   mlr_measures_regr.msle
#'   mlr_measures_regr.medae
#'   mlr_measures_regr.sae
#'   mlr_measures_regr.rsq
#'   mlr_measures_classif.fpr
#'   mlr_measures_regr.sse
#'   mlr_measures_regr.medse
#'   mlr_measures_classif.ppv
#'   mlr_measures_classif.fn
#'   mlr_measures_classif.fp
#'   mlr_measures_classif.fdr
#'   mlr_measures_classif.fomr
#'   mlr_measures_classif.fnr
#'   mlr_measures_classif.mcc
#'   mlr_measures_classif.logloss
#'   mlr_measures_regr.pbias
#'   mlr_measures_regr.maxae
#'   mlr_measures_regr.rmse
#'   mlr_measures_regr.mape
#'   mlr_measures_regr.smape
#'   mlr_measures_regr.rrse
#'   mlr_measures_classif.npv
#'   mlr_measures_classif.acc
#'   mlr_measures_classif.bacc
#'   mlr_measures_regr.bias
#'   mlr_measures_regr.maxse
#'   mlr_measures_regr.rae
#'   mlr_measures_regr.ktau
#'   mlr_measures_regr.mae
#'   mlr_measures_regr.rmsle
#'   mlr_measures_regr.srho
#' @format [R6::R6Class()] inheriting from [Measure].
#'
#' @section Construction:
#' The measures can be retrieved from the dictionary [mlr_measures].
#' With `"<id>"` being the name of the measure to construct:
#' ```
#' mlr_measures$get("<id>")
#' msr("<id>")
#' ```
#'
#' @description
#' The following measures from \CRANpkg{mlr3measures} are interfaced using a simple utility class:
#'
#' Binary classification (inheriting from [MeasureClassif]):
#' * [auc][mlr3measures::auc]: Area Under the ROC Curve
#' * [dor][mlr3measures::dor]: Diagnostic Odds Ratio
#' * [fbeta][mlr3measures::fbeta]: F-beta score
#' * [fdr][mlr3measures::fdr]: False Discovery Rate
#' * [fn][mlr3measures::fn]: False Negatives
#' * [fnr][mlr3measures::fnr]: False Negative Rate
#' * [fomr][mlr3measures::fomr]: False Omission Rate
#' * [fp][mlr3measures::fp]: False Positives
#' * [fpr][mlr3measures::fpr]: False Positive Rate
#' * [mcc][mlr3measures::mcc]: Matthews Correlation Coefficient
#' * [npv][mlr3measures::npv]: Negative Predictive Value
#' * [ppv][mlr3measures::ppv]: Positive Predictive Value
#' * [precision][mlr3measures::precision]: Precision
#' * [recall][mlr3measures::recall]: Recall
#' * [sensitivity][mlr3measures::sensitivity]: Sensitivity
#' * [specificity][mlr3measures::specificity]: Specificity
#' * [tn][mlr3measures::tn]: True Negatives
#' * [tnr][mlr3measures::tnr]: True Negative Rate
#' * [tp][mlr3measures::tp]: True Positives
#' * [tpr][mlr3measures::tpr]: True Positive Rate
#'
#' Multiclass classification (inheriting from [MeasureClassif]):
#' * [acc][mlr3measures::acc]: Classification Accuracy
#' * [bacc][mlr3measures::bacc]: Balanced Classification Accuracy
#' * [ce][mlr3measures::ce]: Classification Error
#' * [logloss][mlr3measures::logloss]: Log Loss
#'
#' Regression (inheriting from [MeasureRegr]):
#' * [bias][mlr3measures::bias]: Bias
#' * [ktau][mlr3measures::ktau]: Kendall's tau
#' * [mae][mlr3measures::mae]: Mean Absolute Errors
#' * [mape][mlr3measures::mape]: Mean Absolute Percent Error
#' * [maxae][mlr3measures::maxae]: Max Absolute Error
#' * [maxse][mlr3measures::maxse]: Max Squared Error
#' * [medae][mlr3measures::medae]: Median Absolute Errors
#' * [medse][mlr3measures::medse]: Median Squared Error
#' * [mse][mlr3measures::mse]: Mean Squared Error
#' * [msle][mlr3measures::msle]: Mean Squared Log Error
#' * [pbias][mlr3measures::pbias]: Percent Bias
#' * [rae][mlr3measures::rae]: Relative Absolute Error
#' * [rmse][mlr3measures::rmse]: Root Mean Squared Error
#' * [rmsle][mlr3measures::rmsle]: Root Mean Squared Log Error
#' * [rrse][mlr3measures::rrse]: Root Relative Squared Error
#' * [rse][mlr3measures::rse]: Relative Squared Error
#' * [rsq][mlr3measures::rsq]: R Squared
#' * [sae][mlr3measures::sae]: Sum of Absolute Errors
#' * [smape][mlr3measures::smape]: Symmetric Mean Absolute Percent Error
#' * [srho][mlr3measures::srho]: Spearman's rho
#' * [sse][mlr3measures::sse]: Sum of Squared Errors
#'
#' Details about the implementation of the respective measures can be found on the corresponding
#' help page in \CRANpkg{mlr3measures}.
#' It is possible to customize the value of each measure which is returned if the measure
#' is not defined for the input by setting the public field `na_value` to an arbitrary `numeric(1)` (default is `NaN`).
#'
#' @template seealso_measure
#' @examples
#' mse = msr("regr.mse")
#' print(mse)
#'
#' # score prediction on mtcars
#' task = tsk("mtcars")
#' pred = lrn("regr.featureless")$train(task)$predict(task)
#' mse$score(pred)
#'
#' # rae: not defined for constant truth
#' pred = PredictionRegr$new(row_ids = 1:10, truth = rep(0, 10), response = runif(10))
#' rae = msr("regr.rae")
#' rae$score(pred)
#'
#' # return worst possible value instead of NaN
#' rae$na_value = rae$range[2]
#' rae$score(pred)
NULL

local({
  .measures = c("tpr", "auc", "mse", "ce", "rse", "tn", "precision", "tp",
    "specificity", "sensitivity", "fbeta", "tnr", "dor", "recall",
    "msle", "medae", "sae", "rsq", "fpr", "sse", "medse", "ppv",
    "fn", "fp", "fdr", "fomr", "fnr", "mcc", "logloss", "pbias",
    "maxae", "rmse", "mape", "smape", "rrse", "npv", "acc", "bacc", "bias",
    "maxse", "rae", "ktau", "mae", "rmsle", "srho")
  stopifnot(all(.measures %in% names(mlr3measures::measures)))

  for (x in mget(.measures, envir = mlr3measures::measures)) {
    switch(x$type,
      "regr" = mlr_measures$add(paste0("regr.", x$id), MeasureRegrSimple, name = x$id),
      "binary" = mlr_measures$add(paste0("classif.", x$id), MeasureBinarySimple, name = x$id),
      "classif" = mlr_measures$add(paste0("classif.", x$id), MeasureClassifSimple, name = x$id),
    )
  }
})

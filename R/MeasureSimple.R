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
        man = paste0("mlr3::mlr_measures_classif.", name)
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
        man = paste0("mlr3::mlr_measures_classif.", name)
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
        man = paste0("mlr3::mlr_measures_regr.", name)
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

### binary classification measures

#' @templateVar id auc
#' @template measure_binary
mlr_measures$add("classif.auc", MeasureBinarySimple, name = "auc")

#' @templateVar id dor
#' @template measure_binary
mlr_measures$add("classif.dor", MeasureBinarySimple, name = "dor")

#' @templateVar id fbeta
#' @template measure_binary
mlr_measures$add("classif.fbeta", MeasureBinarySimple, name = "fbeta")

#' @templateVar id fdr
#' @template measure_binary
mlr_measures$add("classif.fdr", MeasureBinarySimple, name = "fdr")

#' @templateVar id fn
#' @template measure_binary
mlr_measures$add("classif.fn", MeasureBinarySimple, name = "fn")

#' @templateVar id fnr
#' @template measure_binary
mlr_measures$add("classif.fnr", MeasureBinarySimple, name = "fnr")

#' @templateVar id fomr
#' @template measure_binary
mlr_measures$add("classif.fomr", MeasureBinarySimple, name = "fomr")

#' @templateVar id fp
#' @template measure_binary
mlr_measures$add("classif.fp", MeasureBinarySimple, name = "fp")

#' @templateVar id fpr
#' @template measure_binary
mlr_measures$add("classif.fpr", MeasureBinarySimple, name = "fpr")

#' @templateVar id mcc
#' @template measure_binary
mlr_measures$add("classif.mcc", MeasureBinarySimple, name = "mcc")

#' @templateVar id npv
#' @template measure_binary
mlr_measures$add("classif.npv", MeasureBinarySimple, name = "npv")

#' @templateVar id ppv
#' @template measure_binary
mlr_measures$add("classif.ppv", MeasureBinarySimple, name = "ppv")

#' @templateVar id precision
#' @template measure_binary
mlr_measures$add("classif.precision", MeasureBinarySimple, name = "precision")

#' @templateVar id tn
#' @template measure_binary
mlr_measures$add("classif.tn", MeasureBinarySimple, name = "tn")

#' @templateVar id tnr
#' @template measure_binary
mlr_measures$add("classif.tnr", MeasureBinarySimple, name = "tnr")

#' @templateVar id specificity
#' @template measure_binary
mlr_measures$add("classif.specificity", MeasureBinarySimple, name = "specificity")

#' @templateVar id tp
#' @template measure_binary
mlr_measures$add("classif.tp", MeasureBinarySimple, name = "tp")

#' @templateVar id tpr
#' @template measure_binary
mlr_measures$add("classif.tpr", MeasureBinarySimple, name = "tpr")

#' @templateVar id recall
#' @template measure_binary
mlr_measures$add("classif.recall", MeasureBinarySimple, name = "recall")

#' @templateVar id sensitivity
#' @template measure_binary
mlr_measures$add("classif.sensitivity", MeasureBinarySimple, name = "sensitivity")

### multiclass classification measures

#' @templateVar id acc
#' @template measure_classif
mlr_measures$add("classif.acc", MeasureClassifSimple, name = "acc")

#' @templateVar id bacc
#' @template measure_classif
mlr_measures$add("classif.bacc", MeasureClassifSimple, name = "bacc")

#' @templateVar id ce
#' @template measure_classif
mlr_measures$add("classif.ce", MeasureClassifSimple, name = "ce")

#' @templateVar id logloss
#' @template measure_classif
mlr_measures$add("classif.logloss", MeasureClassifSimple, name = "logloss")


### regression measures

#' @templateVar id bias
#' @template measure_regr
mlr_measures$add("regr.bias", MeasureRegrSimple, name = "bias")

#' @templateVar id ktau
#' @template measure_regr
mlr_measures$add("regr.ktau", MeasureRegrSimple, name = "ktau")

#' @templateVar id mae
#' @template measure_regr
mlr_measures$add("regr.mae", MeasureRegrSimple, name = "mae")

#' @templateVar id mape
#' @template measure_regr
mlr_measures$add("regr.mape", MeasureRegrSimple, name = "mape")

#' @templateVar id maxae
#' @template measure_regr
mlr_measures$add("regr.maxae", MeasureRegrSimple, name = "maxae")

#' @templateVar id medae
#' @template measure_regr
mlr_measures$add("regr.medae", MeasureRegrSimple, name = "medae")

#' @templateVar id medse
#' @template measure_regr
mlr_measures$add("regr.medse", MeasureRegrSimple, name = "medse")

#' @templateVar id mse
#' @template measure_regr
mlr_measures$add("regr.mse", MeasureRegrSimple, name = "mse")

#' @templateVar id msle
#' @template measure_regr
mlr_measures$add("regr.msle", MeasureRegrSimple, name = "msle")

#' @templateVar id pbias
#' @template measure_regr
mlr_measures$add("regr.pbias", MeasureRegrSimple, name = "pbias")

#' @templateVar id rae
#' @template measure_regr
mlr_measures$add("regr.rae", MeasureRegrSimple, name = "rae")

#' @templateVar id rmse
#' @template measure_regr
mlr_measures$add("regr.rmse", MeasureRegrSimple, name = "rmse")

#' @templateVar id rmsle
#' @template measure_regr
mlr_measures$add("regr.rmsle", MeasureRegrSimple, name = "rmsle")

#' @templateVar id rrse
#' @template measure_regr
mlr_measures$add("regr.rrse", MeasureRegrSimple, name = "rrse")

#' @templateVar id rse
#' @template measure_regr
mlr_measures$add("regr.rse", MeasureRegrSimple, name = "rse")

#' @templateVar id rsq
#' @template measure_regr
mlr_measures$add("regr.rsq", MeasureRegrSimple, name = "rsq")

#' @templateVar id sae
#' @template measure_regr
mlr_measures$add("regr.sae", MeasureRegrSimple, name = "sae")

#' @templateVar id smape
#' @template measure_regr
mlr_measures$add("regr.smape", MeasureRegrSimple, name = "smape")

#' @templateVar id srho
#' @template measure_regr
mlr_measures$add("regr.srho", MeasureRegrSimple, name = "srho")

#' @templateVar id sse
#' @template measure_regr
mlr_measures$add("regr.sse", MeasureRegrSimple, name = "sse")

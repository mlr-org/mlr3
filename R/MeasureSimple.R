#' @include MeasureClassif.R
MeasureBinarySimple = R6Class("MeasureBinarySimple",
  inherit = MeasureClassif,
  public = list(
    fun = NULL,
    na_value = NaN,
    initialize = function(name, param_set = NULL) {
      if (is.null(param_set)) {
        param_set = ps()
      } else {
        # cloning required because the param set lives in the
        # dictionary mlr_measures
        param_set = param_set$clone()
      }

      info = mlr3measures::measures[[name]]
      super$initialize(
        id = paste0("classif.", name),
        param_set = param_set$clone(),
        range = c(info$lower, info$upper),
        minimize = info$minimize,
        predict_type = info$predict_type,
        task_properties = "twoclass",
        packages = "mlr3measures",
        label = info$title,
        man = paste0("mlr3::mlr_measures_classif.", name)
      )
      self$fun = get(name, envir = asNamespace("mlr3measures"), mode = "function")
    }
  ),

  private = list(
    .score = function(prediction, ...) {
      truth = prediction$truth
      positive = levels(truth)[1L]
      invoke(self$fun, .args = self$param_set$get_values(),
        truth = truth, response = prediction$response, prob = prediction$prob[, positive],
        positive = positive, na_value = self$na_value
      )
    },

    .extra_hash = c("fun", "na_value")
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
        label = info$title,
        man = paste0("mlr3::mlr_measures_classif.", name)
      )
      self$fun = get(name, envir = asNamespace("mlr3measures"), mode = "function")
    }
  ),

  private = list(
    .score = function(prediction, ...) {
      self$fun(truth = prediction$truth, response = prediction$response, prob = prediction$prob, na_value = self$na_value)
    },

    .extra_hash = c("fun", "na_value")
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
        label = info$title,
        man = paste0("mlr3::mlr_measures_regr.", name)
      )
      self$fun = get(name, envir = asNamespace("mlr3measures"), mode = "function")
    }
  ),

  private = list(
    .score = function(prediction, ...) {
      self$fun(truth = prediction$truth, response = prediction$response, se = prediction$se, na_value = self$na_value)
    },

    .extra_hash = c("fun", "na_value")
  )
)

#' @include MeasureSimilarity.R
MeasureSimilaritySimple = R6Class("MeasureSimilaritySimple",
  inherit = MeasureSimilarity,
  public = list(
    fun = NULL,
    na_value = NaN,
    initialize = function(name) {
      info = mlr3measures::measures[[name]]
      self$fun = get(name, envir = asNamespace("mlr3measures"), mode = "function")

      agg = function(rr) {
        sets = map(rr$learners, function(l) l$selected_features())
        self$fun(sets, p = length(rr$task$feature_names))
      }

      super$initialize(
        id = paste0("sim.", name),
        range = c(info$lower, info$upper),
        minimize = info$minimize,
        average = "custom",
        aggregator = agg,
        predict_type = "response",
        packages = "mlr3measures",
        label = info$title,
        man = paste0("mlr3::mlr_measures_sim.", name),
      )
    }
  ),

  private = list(
    .extra_hash = c("fun", "na_value")
  )
)


### binary classification measures

#' @templateVar id auc
#' @template measure_binary
mlr_measures$add("classif.auc", function() MeasureBinarySimple$new(name = "auc"))

#' @templateVar id bbrier
#' @template measure_binary
mlr_measures$add("classif.bbrier", function() MeasureBinarySimple$new(name = "bbrier"))

#' @templateVar id dor
#' @template measure_binary
mlr_measures$add("classif.dor", function() MeasureBinarySimple$new(name = "dor"))

#' @templateVar id fbeta
#' @template measure_binary
mlr_measures$add("classif.fbeta", function() {
  MeasureBinarySimple$new(name = "fbeta", param_set = ps(beta = p_int(lower = 0)))
})

#' @templateVar id fdr
#' @template measure_binary
mlr_measures$add("classif.fdr", function() MeasureBinarySimple$new(name = "fdr"))

#' @templateVar id fn
#' @template measure_binary
mlr_measures$add("classif.fn", function() MeasureBinarySimple$new(name = "fn"))

#' @templateVar id fnr
#' @template measure_binary
mlr_measures$add("classif.fnr", function() MeasureBinarySimple$new(name = "fnr"))

#' @templateVar id fomr
#' @template measure_binary
mlr_measures$add("classif.fomr", function() MeasureBinarySimple$new(name = "fomr"))

#' @templateVar id fp
#' @template measure_binary
mlr_measures$add("classif.fp", function() MeasureBinarySimple$new(name = "fp"))

#' @templateVar id fpr
#' @template measure_binary
mlr_measures$add("classif.fpr", function() MeasureBinarySimple$new(name = "fpr"))

#' @templateVar id mcc
#' @template measure_binary
mlr_measures$add("classif.mcc", function() MeasureBinarySimple$new(name = "mcc"))

#' @templateVar id npv
#' @template measure_binary
mlr_measures$add("classif.npv", function() MeasureBinarySimple$new(name = "npv"))

#' @templateVar id ppv
#' @template measure_binary
mlr_measures$add("classif.ppv", function() MeasureBinarySimple$new(name = "ppv"))

#' @templateVar id prauc
#' @template measure_binary
mlr_measures$add("classif.prauc", function() MeasureBinarySimple$new(name = "prauc"))

#' @templateVar id precision
#' @template measure_binary
mlr_measures$add("classif.precision", function() MeasureBinarySimple$new(name = "precision"))

#' @templateVar id tn
#' @template measure_binary
mlr_measures$add("classif.tn", function() MeasureBinarySimple$new(name = "tn"))

#' @templateVar id tnr
#' @template measure_binary
mlr_measures$add("classif.tnr", function() MeasureBinarySimple$new(name = "tnr"))

#' @templateVar id specificity
#' @template measure_binary
mlr_measures$add("classif.specificity", function() MeasureBinarySimple$new(name = "specificity"))

#' @templateVar id tp
#' @template measure_binary
mlr_measures$add("classif.tp", function() MeasureBinarySimple$new(name = "tp"))

#' @templateVar id tpr
#' @template measure_binary
mlr_measures$add("classif.tpr", function() MeasureBinarySimple$new(name = "tpr"))

#' @templateVar id recall
#' @template measure_binary
mlr_measures$add("classif.recall", function() MeasureBinarySimple$new(name = "recall"))

#' @templateVar id sensitivity
#' @template measure_binary
mlr_measures$add("classif.sensitivity", function() MeasureBinarySimple$new(name = "sensitivity"))

### multiclass classification measures

#' @templateVar id acc
#' @template measure_classif
mlr_measures$add("classif.acc", function() MeasureClassifSimple$new(name = "acc"))

#' @templateVar id bacc
#' @template measure_classif
mlr_measures$add("classif.bacc", function() MeasureClassifSimple$new(name = "bacc"))

#' @templateVar id mbrier
#' @template measure_classif
mlr_measures$add("classif.mbrier", function() MeasureClassifSimple$new(name = "mbrier"))

#' @templateVar id ce
#' @template measure_classif
mlr_measures$add("classif.ce", function() MeasureClassifSimple$new(name = "ce"))

#' @templateVar id logloss
#' @template measure_classif
mlr_measures$add("classif.logloss", function() MeasureClassifSimple$new(name = "logloss"))

#' @templateVar id mauc_aunu
#' @template measure_classif
mlr_measures$add("classif.mauc_aunu", function() MeasureClassifSimple$new(name = "mauc_aunu"))

#' @templateVar id mauc_aunp
#' @template measure_classif
mlr_measures$add("classif.mauc_aunp", function() MeasureClassifSimple$new(name = "mauc_aunp"))

#' @templateVar id mauc_au1u
#' @template measure_classif
mlr_measures$add("classif.mauc_au1u", function() MeasureClassifSimple$new(name = "mauc_au1u"))

#' @templateVar id mauc_au1p
#' @template measure_classif
mlr_measures$add("classif.mauc_au1p", function() MeasureClassifSimple$new(name = "mauc_au1p"))


### regression measures

#' @templateVar id bias
#' @template measure_regr
mlr_measures$add("regr.bias", function() MeasureRegrSimple$new(name = "bias"))

#' @templateVar id ktau
#' @template measure_regr
mlr_measures$add("regr.ktau", function() MeasureRegrSimple$new(name = "ktau"))

#' @templateVar id mae
#' @template measure_regr
mlr_measures$add("regr.mae", function() MeasureRegrSimple$new(name = "mae"))

#' @templateVar id mape
#' @template measure_regr
mlr_measures$add("regr.mape", function() MeasureRegrSimple$new(name = "mape"))

#' @templateVar id maxae
#' @template measure_regr
mlr_measures$add("regr.maxae", function() MeasureRegrSimple$new(name = "maxae"))

#' @templateVar id medae
#' @template measure_regr
mlr_measures$add("regr.medae", function() MeasureRegrSimple$new(name = "medae"))

#' @templateVar id medse
#' @template measure_regr
mlr_measures$add("regr.medse", function() MeasureRegrSimple$new(name = "medse"))

#' @templateVar id mse
#' @template measure_regr
mlr_measures$add("regr.mse", function() MeasureRegrSimple$new(name = "mse"))

#' @templateVar id msle
#' @template measure_regr
mlr_measures$add("regr.msle", function() MeasureRegrSimple$new(name = "msle"))

#' @templateVar id pbias
#' @template measure_regr
mlr_measures$add("regr.pbias", function() MeasureRegrSimple$new(name = "pbias"))

#' @templateVar id rae
#' @template measure_regr
mlr_measures$add("regr.rae", function() MeasureRegrSimple$new(name = "rae"))

#' @templateVar id rmse
#' @template measure_regr
mlr_measures$add("regr.rmse", function() MeasureRegrSimple$new(name = "rmse"))

#' @templateVar id rmsle
#' @template measure_regr
mlr_measures$add("regr.rmsle", function() MeasureRegrSimple$new(name = "rmsle"))

#' @templateVar id rrse
#' @template measure_regr
mlr_measures$add("regr.rrse", function() MeasureRegrSimple$new(name = "rrse"))

#' @templateVar id rse
#' @template measure_regr
mlr_measures$add("regr.rse", function() MeasureRegrSimple$new(name = "rse"))

#' @templateVar id rsq
#' @template measure_regr
mlr_measures$add("regr.rsq", function() MeasureRegrSimple$new(name = "rsq"))

#' @templateVar id sae
#' @template measure_regr
mlr_measures$add("regr.sae", function() MeasureRegrSimple$new(name = "sae"))

#' @templateVar id smape
#' @template measure_regr
mlr_measures$add("regr.smape", function() MeasureRegrSimple$new(name = "smape"))

#' @templateVar id srho
#' @template measure_regr
mlr_measures$add("regr.srho", function() MeasureRegrSimple$new(name = "srho"))

#' @templateVar id sse
#' @template measure_regr
mlr_measures$add("regr.sse", function() MeasureRegrSimple$new(name = "sse"))


### similarity measures

#' @templateVar id jaccard
#' @template measure_similarity
mlr_measures$add("sim.jaccard", function() MeasureSimilaritySimple$new(name = "jaccard"))

#' @templateVar id phi
#' @template measure_similarity
mlr_measures$add("sim.phi", function() MeasureSimilaritySimple$new(name = "phi"))

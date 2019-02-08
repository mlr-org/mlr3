#' @title Featureless Classification Learner
#'
#' @name mlr_learners_classif.featureless
#' @description
#' A simple [LearnerClassif] which only analyses the labels during train, ignoring all features.
#' Hyperparameter `method` determines the mode of operation during prediction:
#' \describe{
#'   \item{mode:}{Predicts the most frequent label. If there are two or more labels tied, randomly selects one per prediction.}
#'   \item{sample:}{Randomly predict a label uniformly.}
#'   \item{weighed.sample:}{Randomly predict a label, with probability estimated from the training distribution.}
#' }
#' @include LearnerClassif.R
#' @export
LearnerClassifFeatureless = R6Class("LearnerClassifFeatureless", inherit = LearnerClassif,
  public = list(
    initialize = function(id = "featureless", param_vals = list(method = "mode"), predict_type = "response") {
      super$initialize(
        id = id,
        feature_types = c("logical", "integer", "numeric", "character", "factor", "ordered"),
        predict_type = predict_type,
        predict_types = c("response", "prob"),
        param_set = ParamSet$new(
          params = list(
            ParamFct$new("method", levels = c("mode", "sample", "weighted.sample"), default = "mode", tags = "predict")
          )
        ),
        param_vals = param_vals,
        properties = c("twoclass", "multiclass",  "missings", "importance", "selected_features")
      )
    },

    train = function(task) {
      tn = task$target_names
      self$model = set_class(list(tab = table(task$data(cols = tn)[[1L]]), features = task$feature_names), "featureless")
      self
    },

    predict = function(task) {
      pv = self$params("predict")
      tab = self$model$tab
      n = task$nrow
      response = prob = NULL

      if (self$predict_type == "response") {
        response = switch(pv$method,
          mode = rep.int(sample(names(tab[tab == max(tab)]), 1L), n),
          sample = sample(names(tab), n, replace = TRUE),
          weighted.sample = sample(names(tab), n, replace = TRUE, prob = tab)
        )
      } else if (self$predict_type == "prob") {
        prob = switch(pv$method,
          mode = { tmp = (tab == max(tab)); tmp / sum(tmp) },
          sample = rep.int(1 / length(tab), length(tab)),
          weighted.sample = tab / sum(tab)
        )
        prob = matrix(prob, nrow = n, ncol = length(tab), byrow = TRUE)
        colnames(prob) = names(tab)
      }

      PredictionClassif$new(task, response = response, prob = prob)
    },

    importance = function() {
      if (is.null(self$model))
        stopf("No model stored")
      fn = self$model$features
      set_names(double(length(fn)), fn)
    },

    selected_features = function() {
      character(0L)
    }
  )
)

#' @include mlr_learners.R
mlr_learners$add("classif.featureless", LearnerClassifFeatureless)

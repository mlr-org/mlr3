#' @title Featureless Classification Learner
#'
#' @aliases mlr_learners_classif.featureless
#' @format [R6::R6Class] inheriting from [LearnerClassif].
#' @include LearnerClassif.R
#'
#' @description
#' A simple [LearnerClassif] which only analyses the labels during train, ignoring all features.
#' Hyperparameter `method` determines the mode of operation during prediction:
#' \describe{
#'   \item{mode:}{Predicts the most frequent label. If there are two or more labels tied, randomly selects one per prediction.}
#'   \item{sample:}{Randomly predict a label uniformly.}
#'   \item{weighed.sample:}{Randomly predict a label, with probability estimated from the training distribution.}
#' }
#' @export
LearnerClassifFeatureless = R6Class("LearnerClassifFeatureless", inherit = LearnerClassif,
  public = list(
    initialize = function(id = "classif.featureless") {
      super$initialize(
        id = id,
        feature_types = c("logical", "integer", "numeric", "character", "factor", "ordered"),
        predict_types = c("response", "prob"),
        param_set = ParamSet$new(
          params = list(
            ParamFct$new("method", levels = c("mode", "sample", "weighted.sample"), default = "mode", tags = "predict")
          )
        ),
        param_vals = list(method = "mode"),
        properties = c("twoclass", "multiclass", "missings", "importance", "selected_features")
      )
    },

    train_internal = function(task) {
      tn = task$target_names
      set_class(list(tab = table(task$data(cols = tn)[[1L]]), features = task$feature_names), "classif.featureless_model")
    },

    predict_internal = function(task) {
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
        list(response = response)
      } else {
        prob = switch(pv$method,
          mode = {
            tmp = (tab == max(tab))
            tmp / sum(tmp)
          },
          sample = rep.int(1 / length(tab), length(tab)),
          weighted.sample = tab / sum(tab)
        )
        prob = matrix(prob, nrow = n, ncol = length(tab), byrow = TRUE)
        colnames(prob) = names(tab)
        list(prob = prob)
      }
    },

    importance = function() {
      if (is.null(self$model)) {
        stopf("No model stored")
      }
      fn = self$model$features
      set_names(double(length(fn)), fn)
    },

    selected_features = function() {
      character(0L)
    }
  )
)

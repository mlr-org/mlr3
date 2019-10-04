#' @title Featureless Classification Learner
#'
#' @usage NULL
#' @aliases mlr_learners_classif.featureless
#' @format [R6::R6Class] inheriting from [LearnerClassif].
#' @include LearnerClassif.R
#'
#' @section Construction:
#' ```
#' LearnerClassifFeatureless$new()
#' mlr_learners$get("classif.featureless")
#' lrn("classif.featureless")
#' ```
#'
#' @description
#' A simple [LearnerClassif] which only analyses the labels during train, ignoring all features.
#' Hyperparameter `method` determines the mode of operation during prediction:
#' \describe{
#'   \item{mode:}{Predicts the most frequent label. If there are two or more labels tied, randomly selects one per prediction.}
#'   \item{sample:}{Randomly predict a label uniformly.}
#'   \item{weighed.sample:}{Randomly predict a label, with probability estimated from the training distribution.}
#' }
#' @template seealso_learner
#' @export
LearnerClassifFeatureless = R6Class("LearnerClassifFeatureless", inherit = LearnerClassif,
  public = list(
    initialize = function() {
      ps = ParamSet$new(list(ParamFct$new("method", levels = c("mode", "sample", "weighted.sample"), default = "mode", tags = "predict")))
      ps$values = list(method = "mode")
      super$initialize(
        id = "classif.featureless",
        feature_types = c("logical", "integer", "numeric", "character", "factor", "ordered"),
        predict_types = c("response", "prob"),
        param_set = ps,
        properties = c("twoclass", "multiclass", "missings", "importance", "selected_features"),
        man = "mlr3::mlr_learners_classif.featureless"
      )
    },

    train_internal = function(task) {
      tn = task$target_names
      set_class(list(tab = table(task$data(cols = tn)[[1L]]), features = task$feature_names), "classif.featureless_model")
    },

    predict_internal = function(task) {
      pv = self$param_set$get_values(tags = "predict")
      tab = self$model$tab
      n = task$nrow
      response = prob = NULL

      if (self$predict_type == "response") {
        response = switch(pv$method,
          mode = rep.int(sample(names(tab[tab == max(tab)]), 1L), n),
          sample = sample(names(tab), n, replace = TRUE),
          weighted.sample = sample(names(tab), n, replace = TRUE, prob = tab)
        )
        PredictionClassif$new(task, response = response)
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
        PredictionClassif$new(task = task, prob = prob)
      }
    },

    importance = function() {
      if (is.null(self$model)) {
        stopf("No model stored")
      }
      fn = self$model$features
      named_vector(fn, 0)
    },

    selected_features = function() {
      character()
    }
  )
)

#' @include mlr_learners.R
mlr_learners$add("classif.featureless", LearnerClassifFeatureless)

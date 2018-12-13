#' @title Featureless Classification Learner
#' @name mlr_learners_classif_featureless
#' @format [R6::R6Class()] inheriting from [LearnerClassif].
#' @description
#' A simple learner which only analyses the labels during train, ignoring all features.
#' Hyperparameter `method` determines the mode of operation during prediction:
#' \describe{
#'   \item{mode:}{Predicts the most frequent label. If there are two or more labels tied, randomly selects one per prediction.}
#'   \item{sample:}{Randomly predict a label uniformly.}
#'   \item{weighed.sample:}{Randomly predict a label, with probability estimated from the training distribution.}
#' }
#' @export
#' @include LearnerClassif.R
LearnerClassifFeatureless = R6Class("LearnerClassifFeatureless", inherit = LearnerClassif,
  public = list(
    initialize = function(id = "classif.featureless") {
      super$initialize(
        id = id,
        feature_types = c("logical", "integer", "numeric", "character", "factor", "ordered"),
        predict_types = c("response", "prob"),
        param_set = ParamSet$new(
          params = list(
            ParamFct$new("method", values = c("mode", "sample", "weighted.sample"), default = "mode", tags = "predict")
          )
        ),
        param_vals = list(method = "mode"),
        properties = "missings",
      )
    },

    train = function(task) {
      tn = task$target_names
      model = table(task$data(cols = tn))
      class(model) = "featureless"
      model
    },

    predict = function(model, task) {
      n = task$nrow
      response = prob = NULL

      if (self$predict_type == "response") {
        response = switch(self$param_vals$method,
          mode = rep.int(sample(names(model[model == max(model)]), 1L), n),
          sample = sample(names(model), n, replace = TRUE),
          weighted.sample = sample(names(model), n, replace = TRUE, prob = model)
        )
      } else if (self$predict_type == "prob") {
        prob = switch(self$param_vals$method,
          mode = { tmp = (model == max(model)); tmp / sum(tmp) },
          sample = rep.int(1 / length(model), length(model)),
          weighted.sample = model / sum(model)
        )
        prob = matrix(prob, nrow = n, ncol = length(model), byrow = TRUE)
        colnames(prob) = names(model)
      }

      PredictionClassif$new(task, response = response, prob = prob)
    }
  )
)

#' @include mlr_learners.R
mlr_learners$add("classif.featureless", LearnerClassifFeatureless)

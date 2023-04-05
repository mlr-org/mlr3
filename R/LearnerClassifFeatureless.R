#' @title Featureless Classification Learner
#'
#' @name mlr_learners_classif.featureless
#' @include LearnerClassif.R
#'
#' @description
#' A simple [LearnerClassif] which only analyzes the labels during train, ignoring all features.
#' Hyperparameter `method` determines the mode of operation during prediction:
#' \describe{
#'   \item{mode:}{
#'     Predicts the most frequent label. If there are two or more labels tied, randomly selects one per prediction.
#'     Probabilities correspond to the relative frequency of the class labels in the training set.
#'   }
#'   \item{sample:}{
#'     Randomly predict a label uniformly.
#'     Probabilities correspond to a uniform distribution of class labels, i.e. 1 divided by the number of classes.
#'   }
#'   \item{weighted.sample:}{
#'     Randomly predict a label, with probability estimated from the training distribution.
#'     For consistency, probabilities are 1 for the sampled label and 0 for all other labels.
#'   }
#' }
#'
#' @templateVar id classif.featureless
#' @template learner
#'
#' @template seealso_learner
#' @export
LearnerClassifFeatureless = R6Class("LearnerClassifFeatureless", inherit = LearnerClassif,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      ps = ps(
        method = p_fct(c("mode", "sample", "weighted.sample"), default = "mode", tags = "predict")
      )
      ps$values = list(method = "mode")
      super$initialize(
        id = "classif.featureless",
        feature_types = mlr_reflections$task_feature_types,
        predict_types = c("response", "prob"),
        param_set = ps,
        properties = c("featureless", "twoclass", "multiclass", "missings", "importance", "selected_features"),
        label = "Featureless Classification Learner",
        man = "mlr3::mlr_learners_classif.featureless",
      )
    },

    #' @description
    #' All features have a score of `0` for this learner.
    #' @return Named `numeric()`.
    importance = function() {
      if (is.null(self$model)) {
        stopf("No model stored")
      }
      fn = self$model$features
      named_vector(fn, 0)
    },

    #' @description
    #' Selected features are always the empty set for this learner.
    #' @return `character(0)`.
    selected_features = function() {
      character()
    }
  ),

  private = list(
    .train = function(task) {
      tn = task$target_names
      set_class(list(tab = table(task$data(cols = tn)[[1L]]), features = task$feature_names), "classif.featureless_model")
    },

    .predict = function(task) {
      pv = self$param_set$get_values(tags = "predict")
      tab = self$model$tab
      n = task$nrow
      labels = names(tab)
      response = prob = NULL

      if (self$predict_type == "response") {
        response = switch(pv$method,
          mode = sample(labels[tab == max(tab)], n, replace = TRUE),
          sample = sample(labels, n, replace = TRUE),
          weighted.sample = sample(labels, n, replace = TRUE, prob = tab)
        )
      } else if (self$predict_type == "prob") {
        prob = switch(pv$method,
          mode = matrix(unname(tab) / sum(tab), nrow = n, ncol = length(tab), byrow = TRUE),
          sample = matrix(1 / length(tab), nrow = n, ncol = length(tab)),
          weighted.sample = diag(length(tab))[sample(seq_along(tab), n, replace = TRUE, prob = tab),, drop = FALSE]
        )
        colnames(prob) = labels
      }

      list(response = response, prob = prob)
    }
  )
)

#' @include mlr_learners.R
mlr_learners$add("classif.featureless", function() LearnerClassifFeatureless$new())

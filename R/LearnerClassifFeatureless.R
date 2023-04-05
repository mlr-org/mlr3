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
#'     For consistency, probabilities are 1 for the predicted label and 0 for all other labels.
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
      response = prob = NULL

      response = switch(pv$method,
        mode = rep.int(sample(names(tab[tab == max(tab)]), 1L), n),
        sample = sample(names(tab), n, replace = TRUE),
        weighted.sample = sample(names(tab), n, replace = TRUE, prob = tab)
      )

      if (self$predict_type == "prob") {
        prob = switch(pv$method,
          mode = unname(tab) / sum(tab),
          sample = rep.int(1 / length(tab), length(tab)),
          weighted.sample = diag(length(tab))[match(response, names(tab)), ]
        )
        if (!is.matrix(prob)) {
          prob = matrix(prob, nrow = n, ncol = length(tab), byrow = TRUE)
        }
        colnames(prob) = names(tab)
      }

      list(response = response, prob = prob)
    }
  )
)

#' @include mlr_learners.R
mlr_learners$add("classif.featureless", function() LearnerClassifFeatureless$new())

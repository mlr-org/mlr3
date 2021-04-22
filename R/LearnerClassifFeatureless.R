#' @title Featureless Classification Learner
#'
#' @name mlr_learners_classif.featureless
#' @include LearnerClassif.R
#'
#' @description
#' A simple [LearnerClassif] which only analyses the labels during train, ignoring all features.
#' Hyperparameter `method` determines the mode of operation during prediction:
#' \describe{
#'   \item{mode:}{Predicts the most frequent label. If there are two or more labels tied, randomly selects one per prediction.}
#'   \item{sample:}{Randomly predict a label uniformly.}
#'   \item{weighted.sample:}{Randomly predict a label, with probability estimated from the training distribution.}
#' }
#'
#' @templateVar id classif.featureless
#' @template section_dictionary_learner
#'
#' @section Meta Information:
#' `r rd_info(lrn("classif.featureless"))`
#'
#' @section Parameters:
#' `r rd_info(lrn("classif.featureless")$param_set)`
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
        man = "mlr3::mlr_learners_classif.featureless"
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
    }
  )
)

#' @include mlr_learners.R
mlr_learners$add("classif.featureless", LearnerClassifFeatureless)

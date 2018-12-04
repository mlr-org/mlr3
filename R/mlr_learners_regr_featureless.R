#' @include LearnerRegr.R
LearnerRegrDummy = R6Class("LearnerRegrDummy", inherit = LearnerRegr,
  public = list(
    initialize = function(id = "regr.featureless") {
      super$initialize(
        id = id,
        feature_types = c("logical", "integer", "numeric", "character", "factor", "ordered"),
        predict_types = c("response", "se"),
        param_set = ParamSet$new(
          params = list(
            ParamLgl$new("robust", default = TRUE)
          )
        ),
        properties = "missings",
      )
    },

    train = function(task, robust = TRUE, ...) {
      tn = unlist(task$data(cols = task$target_names))
      mod = if (isTRUE(robust)) c(mean(tn), sd(tn)) else c(median(tn), madn(tn))
      class(mod) = "featureless"
      mod
    },

    predict = function(model, task, ...) {
      n = task$nrow
      PredictionRegr$new(task, response = rep(model[1L], n), se = rep(model[2L], n))
    }
  )
)

#' @include mlr_learners.R
mlr_learners$add("regr.featureless", LearnerRegrDummy)

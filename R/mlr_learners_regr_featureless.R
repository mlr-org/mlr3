#' @title Featureless Regression Learner
#' @name mlr_learners_regr_featureless
#' @format [R6::R6Class] inheriting from [LearnerRegr].
#' @description
#' A simple learner which only analyses the response during train, ignoring all features.
#' If hyperparameter `robust` is `FALSE` (default), constantly predicts `mean(y)` as response
#' and `sd(y)` as standard error.
#' If `robust` is `TRUE`, `median()` and `madn()` are used instead of `mean()` and `sd()`,
#' respectively.
#' @export
#' @references [HTML help page](https://mlr3.mlr-org.com/reference/mlr_learners_regr_featureless.html)
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
            ParamLgl$new("robust", default = TRUE, tags = "train")
          )
        ),
        param_vals = list(robust = TRUE),
        properties = "missings",
      )
    },

    train = function(task) {
      tn = unlist(task$data(cols = task$target_names))
      mod = if (isTRUE(self$param_vals$robust)) c(mean(tn), sd(tn)) else c(median(tn), madn(tn))
      self$model = set_class(mod, "featureless")
      self
    },

    predict = function(task) {
      n = task$nrow
      PredictionRegr$new(task, response = rep(self$model[1L], n), se = rep(self$model[2L], n))
    }
  )
)

#' @include mlr_learners.R
mlr_learners$add("regr.featureless", LearnerRegrDummy)

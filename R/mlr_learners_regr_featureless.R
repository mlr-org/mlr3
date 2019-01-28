#' @title Featureless Regression Learner
#'
#' @name mlr_learners_regr.featureless
#' @format [R6::R6Class] inheriting from [LearnerRegr].
#' @description
#' A simple [LearnerRegr] which only analyses the response during train, ignoring all features.
#' If hyperparameter `robust` is `FALSE` (default), constantly predicts `mean(y)` as response
#' and `sd(y)` as standard error.
#' If `robust` is `TRUE`, `median()` and `madn()` are used instead of `mean()` and `sd()`,
#' respectively.
#' @include LearnerRegr.R
#' @export
LearnerRegrFeatureless = R6Class("LearnerRegrFeatureless", inherit = LearnerRegr,
  public = list(
    initialize = function() {
      super$initialize(
        id = "featureless",
        feature_types = c("logical", "integer", "numeric", "character", "factor", "ordered"),
        predict_types = c("response", "se"),
        param_set = ParamSet$new(
          params = list(
            ParamLgl$new("robust", default = TRUE, tags = "train")
          )
        ),
        param_vals = list(robust = FALSE),
        properties = "missings",
      )
    },

    train = function(task) {
      x = task$data(cols = task$target_names)[[1L]]
      if (isFALSE(self$param_vals$robust)) {
        location = mean(x)
        dispersion = sd(x)
      } else {
        location = median(x)
        dispersion = mad(x, center = location)
      }
      self$model = set_class(list(location = location, dispersion = dispersion, features = task$feature_names), "featureless")
      self
    },

    predict = function(task) {
      n = task$nrow
      PredictionRegr$new(task, response = rep(self$model$location, n), se = rep(self$model$dispersion, n))
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
mlr_learners$add("regr.featureless", LearnerRegrFeatureless)

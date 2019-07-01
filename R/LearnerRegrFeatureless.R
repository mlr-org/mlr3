#' @title Featureless Regression Learner
#'
#' @aliases mlr_learners_regr.featureless
#' @format [R6::R6Class] inheriting from [LearnerRegr].
#' @include LearnerRegr.R
#'
#' @description
#' A simple [LearnerRegr] which only analyses the response during train, ignoring all features.
#' If hyperparameter `robust` is `FALSE` (default), constantly predicts `mean(y)` as response
#' and `sd(y)` as standard error.
#' If `robust` is `TRUE`, `median()` and `madn()` are used instead of `mean()` and `sd()`,
#' respectively.
#' @export
LearnerRegrFeatureless = R6Class("LearnerRegrFeatureless", inherit = LearnerRegr,
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
        param_vals = list(robust = FALSE),
        properties = c("missings", "importance", "selected_features"),
        packages = "stats"
      )
    },

    train_internal = function(task) {
      pv = self$param_set$get_values(tag = "train")
      x = task$data(cols = task$target_names)[[1L]]
      if (isFALSE(pv$robust)) {
        location = mean(x)
        dispersion = sd(x)
      } else {
        location = stats::median(x)
        dispersion = stats::mad(x, center = location)
      }
      set_class(list(location = location, dispersion = dispersion, features = task$feature_names), "regr.featureless_model")
    },

    predict_internal = function(task) {
      n = task$nrow
      response = rep(self$model$location, n)
      se = if (self$predict_type == "se") rep(self$model$dispersion, n) else NULL
      list(response = response, se = se)
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

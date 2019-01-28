#' @title Regression Tree Learner
#'
#' @name mlr_learners_regr.rpart
#' @format [R6::R6Class] inheriting from [LearnerRegr].
#' @description
#' A learner for a regression tree implemented in [rpart::rpart].
#' @include LearnerRegr.R
#' @export
LearnerRegrRpart = R6Class("LearnerRegrRpart", inherit = LearnerRegr,
  public = list(
    initialize = function() {
      super$initialize(
        id = "rpart",
        feature_types = c("logical", "integer", "numeric", "character", "factor", "ordered"),
        predict_types = "response",
        packages = "rpart",
        param_set = ParamSet$new(
          params = list(
            ParamInt$new(id = "minsplit", default = 20L, lower = 1L, tags = "train"),
            ParamDbl$new(id = "cp", default = 0.01, lower = 0, upper = 1, tags = "train"),
            ParamInt$new(id = "maxcompete", default = 4L, lower = 0L, tags = "train"),
            ParamInt$new(id = "maxsurrogate", default = 5L, lower = 0L, tags = "train"),
            ParamInt$new(id = "maxdepth", default = 30L, lower = 1L, upper = 30L, tags = "train"),
            ParamInt$new(id = "xval", default = 10L, lower = 0L, tags = "train")
          )
        ),
        properties = c("missings", "importance")
      )
    },

    train = function(task) {
      pars = self$params_train
      self$model = invoke(rpart::rpart, formula = task$formula, data = task$data(), .args = pars)
      self
    },

    predict = function(task) {
      newdata = task$data(cols = task$feature_names)
      response = predict(self$model, newdata = newdata)
      PredictionRegr$new(task, response = response)
    },

    importance = function() {
      if (is.null(self$model))
        stopf("No model stored")
      sort(self$model$variable.importance, decreasing = TRUE)
    },

    selected_features = function() {
      if (is.null(self$model))
        stopf("No model stored")
      unique(setdiff(self$model$frame$var, "<leaf>"))
    }
  )
)

#' @include mlr_learners.R
mlr_learners$add("regr.rpart", LearnerRegrRpart)

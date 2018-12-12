#' @title Regression Tree Learner
#' @name mlr_learners_regr_rpart
#' @format [R6::R6Class()] inheriting from [LearnerRegr].
#' @description
#' A learner for a regression tree implemented in package \pkg{rpart}.
#' @keywords internal
#' @export
#' @include LearnerRegr.R
LearnerRegrRpart = R6Class("LearnerRegrRpart", inherit = LearnerRegr,
  public = list(
    initialize = function(id = "regr.rpart") {
      super$initialize(
        id = id,
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
        properties = "missings"
      )
    },

    train = function(task) {
      pars = self$params_train
      invoke(rpart::rpart, pars, formula = task$formula, data = task$data())
    },

    predict = function(model, task) {
      newdata = task$data()
      response = predict(model, newdata = newdata)
      PredictionRegr$new(task, response = response)
    }
  )
)

#' @include mlr_learners.R
mlr_learners$add("regr.rpart", LearnerRegrRpart)

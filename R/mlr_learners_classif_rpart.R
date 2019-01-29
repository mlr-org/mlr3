#' @title Classification Tree Learner
#'
#' @name mlr_learners_classif.rpart
#' @format [R6::R6Class] inheriting from [LearnerClassif].
#' @description
#' A [LearnerClassif] for a classification tree implemented in [rpart::rpart].
#' @include LearnerClassif.R
#' @export
LearnerClassifRpart = R6Class("LearnerClassifRpart", inherit = LearnerClassif,
  public = list(
    initialize = function() {
      super$initialize(
        id = "rpart",
        packages = "rpart",
        feature_types = c("logical", "integer", "numeric", "character", "factor", "ordered"),
        predict_types = c("response", "prob"),
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
        properties = c("twoclass", "multiclass", "missings", "importance", "selected_features")
      )
    },

    train = function(task) {
      pars = self$params("train")
      self$model = invoke(rpart::rpart, formula = task$formula, data = task$data(), .args = pars)
      self
    },

    predict = function(task) {
      newdata = task$data(cols = task$feature_names)
      response = prob = NULL

      if (self$predict_type == "response") {
        response = as.character(predict(self$model, newdata = newdata, type = "class"))
      } else if (self$predict_type == "prob") {
        prob = predict(self$model, newdata = newdata, type = "prob")
      }

      PredictionClassif$new(task, response, prob)
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
mlr_learners$add("classif.rpart", LearnerClassifRpart)

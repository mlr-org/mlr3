#' @title Classification Tree Learner
#'
#' @name mlr_learners_classif.rpart
#' @format [R6::R6Class] inheriting from [LearnerClassif].
#' @include LearnerClassif.R
#'
#' @description
#' A [LearnerClassif] for a classification tree implemented in [rpart::rpart()] in package \CRANpkg{rpart}.
#'
#' @references
#' Breiman, L. (1984).
#' Classification and Regression Trees.
#' New York: Routledge.
#' \url{https://doi.org/10.1201/9781315139470}
#'
#' @export
LearnerClassifRpart = R6Class("LearnerClassifRpart", inherit = LearnerClassif,
  public = list(
    initialize = function(id = "classif.rpart") {
      super$initialize(
        id = id,
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
        properties = c("twoclass", "multiclass", "weights", "missings", "importance", "selected_features")
      )
    },

    train = function(task) {
      pars = self$params("train")
      if ("weights" %in% task$properties)
        pars = insert_named(pars, list(weights = task$weights$weight))
      self$model = invoke(rpart::rpart, formula = task$formula(), data = task$data(), .args = pars)
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
      # importance is only present if there is at least on split
      sort(self$model$variable.importance %??% set_names(numeric()), decreasing = TRUE)
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

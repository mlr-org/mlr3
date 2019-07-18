#' @title Classification Tree Learner
#'
#' @aliases mlr_learners_classif.rpart
#' @format [R6::R6Class] inheriting from [LearnerClassif].
#' @include LearnerClassif.R
#'
#' @description
#' A [LearnerClassif] for a classification tree implemented in [rpart::rpart()] in package \CRANpkg{rpart}.
#' Parameter `xval` is set to 0 in order to save some computation time.
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
        param_vals = list(xval = 0L),
        properties = c("twoclass", "multiclass", "weights", "missings", "importance", "selected_features")
      )
    },

    train_internal = function(task) {
      pv = self$param_set$get_values(tags = "train")
      if ("weights" %in% task$properties) {
        pv = insert_named(pv, list(weights = task$weights$weight))
      }
      invoke(rpart::rpart, formula = task$formula(), data = task$data(), .args = pv, .opts = allow_partial_matching)
    },

    predict_internal = function(task) {
      newdata = task$data(cols = task$feature_names)
      response = prob = NULL

      if (self$predict_type == "response") {
        response = invoke(predict, self$model, newdata = newdata, type = "class", .opts = allow_partial_matching)
        # response = as.character(response)
      } else if (self$predict_type == "prob") {
        prob = invoke(predict, self$model, newdata = newdata, type = "prob", .opts = allow_partial_matching)
      }

      PredictionClassif$new(task = task, response = response, prob = prob)
    },

    importance = function() {
      if (is.null(self$model)) {
        stopf("No model stored")
      }
      # importance is only present if there is at least on split
      sort(self$model$variable.importance %??% set_names(numeric()), decreasing = TRUE)
    },

    selected_features = function() {
      if (is.null(self$model)) {
        stopf("No model stored")
      }
      unique(setdiff(self$model$frame$var, "<leaf>"))
    }
  )
)

#' @include mlr_learners.R
mlr_learners$add("classif.rpart", LearnerClassifRpart)

#' @title Regression Tree Learner
#'
#' @usage NULL
#' @name mlr_learners_regr.rpart
#' @format [R6::R6Class] inheriting from [LearnerRegr].
#' @include LearnerRegr.R
#'
#' @section Construction:
#' ```
#' LearnerRegrRpart$new()
#' mlr_learners$get("regr.rpart")
#' lrn("regr.rpart")
#' ```
#'
#' @description
#' A [LearnerRegr] for a regression tree implemented in [rpart::rpart()] in package \CRANpkg{rpart}.
#' Parameter `xval` is set to 0 in order to save some computation time.
#'
#' @references
#' \cite{mlr3}{breiman_1984}
#'
#' @template seealso_learner
#' @export
LearnerRegrRpart = R6Class("LearnerRegrRpart", inherit = LearnerRegr,
  public = list(
    initialize = function() {
      ps = ParamSet$new(list(
        ParamInt$new(id = "minsplit", default = 20L, lower = 1L, tags = "train"),
        ParamDbl$new(id = "cp", default = 0.01, lower = 0, upper = 1, tags = "train"),
        ParamInt$new(id = "maxcompete", default = 4L, lower = 0L, tags = "train"),
        ParamInt$new(id = "maxsurrogate", default = 5L, lower = 0L, tags = "train"),
        ParamInt$new(id = "maxdepth", default = 30L, lower = 1L, upper = 30L, tags = "train"),
        ParamInt$new(id = "xval", default = 10L, lower = 0L, tags = "train")
      ))
      ps$values = list(xval = 0L)

      super$initialize(
        id = "regr.rpart",
        feature_types = c("logical", "integer", "numeric", "factor", "ordered"),
        predict_types = "response",
        packages = "rpart",
        param_set = ps,
        properties = c("weights", "missings", "importance", "selected_features"),
        man = "mlr3::mlr_learners_regr.rpart"
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
      response = invoke(predict, self$model, newdata = newdata, .opts = allow_partial_matching)
      PredictionRegr$new(task = task, response = response)
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
mlr_learners$add("regr.rpart", LearnerRegrRpart)

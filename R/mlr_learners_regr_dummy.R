#' @include LearnerRegr.R
LearnerRegrDummy = R6Class("LearnerRegrDummy", inherit = LearnerRegr,
  public = list(
    initialize = function(id = "regr.dummy") {
      super$initialize(
        id = id,
        feature_types = c("logical", "integer", "numeric", "character", "factor", "ordered"),
        predict_types = c("response", "se"),
        par_set = ParamSet$new(
          params = list(
            ParamFlag$new("robust", default = TRUE)
          )
        ),
        properties = "missings",
      )
    },

    train = function(task, robust = TRUE, ...) {
      tn = unlist(task$data(cols = task$target_names))
      mod = if (isTRUE(robust)) c(mean(tn), sd(tn)) else c(median(tn), madn(tn))
      class(mod) = "dummy.model"
      mod
    },

    predict = function(model, task, ...) {
      n = task$nrow
      PredictionRegr$new(
        task = task,
        response = rep(model[1L], n),
        se = rep(model[2L], n)
      )
    }
  )
)

#' @include mlr_learners.R
mlr_learners$add("regr.dummy", LearnerRegrDummy)

#' @include LearnerRegr.R
LearnerRegrRpart = R6Class("LearnerRegrRpart", inherit = LearnerRegr,
  public = list(
    initialize = function() {
      super$initialize(
        id = "regr.rpart",
        feature_types = c("logical", "integer", "numeric", "character", "factor", "ordered"),
        predict_types = "response",
        packages = "rpart",
        par_set = ParamSet$new(
          params = list(
            ParamInt$new(id = "minsplit", default = 20L, lower = 1L),
            ParamReal$new(id = "cp", default = 0.01, lower = 0, upper = 1),
            ParamInt$new(id = "maxcompete", default = 4L, lower = 0L),
            ParamInt$new(id = "maxsurrogate", default = 5L, lower = 0L),
            ParamInt$new(id = "maxdepth", default = 30L, lower = 1L, upper = 30L),
            ParamInt$new(id = "xval", default = 10L, lower = 0L)
          )
        ),
        properties = "missings"
      )
    },

    train = function(task, ...) {
      rpart::rpart(task$formula, task$data(), ...)
    },

    predict = function(model, task, ...) {
      newdata = task$data(cols = task$feature_names)
      response = predict(model, newdata = newdata, ...)
      PredictionRegr$new(task, response = response)
    }
  )
)

#' @include mlr_learners.R
mlr_learners$add("regr.rpart", LearnerRegrRpart)

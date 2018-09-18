#' @include LearnerClassif.R
LearnerClassifRpart = R6Class("LearnerClassifRpart", inherit = LearnerClassif,
  public = list(
    initialize = function() {
      super$initialize(
        id = "classif.rpart",
        packages = "rpart",
        par_set = ParamSet$new(
          params = list(
            ParamInt$new(id = "minsplit", default = 20L, lower = 1L),
            ParamReal$new(id = "cp", default = 0.01, lower = 0, upper = 1),
            ParamInt$new(id = "maxcompete", default = 4L, lower = 0L),
            ParamInt$new(id = "maxsurrogate", default = 5L, lower = 0L), ParamInt$new(id = "maxdepth", default = 30L, lower = 1L, upper = 30L),
            ParamInt$new(id = "xval", default = 10L, lower = 0L)
          )
        ),
        properties = c("twoclass", "multiclass", "missings", "feat.numeric", "feat.factor", "feat.ordered", "prob")
      )
    },

    train = function(task, ...) {
      self$model = rpart::rpart(task$formula, task$data(), ...)
    },

    predict = function(model, task, ...) {
      newdata = task$data(cols = task$feature_names)
      response = as.character(predict(model, newdata = newdata, type = "class"))
      prob = if (self$predict_type == "response") NULL else predict(model, newdata = newdata, type = "prob")

      PredictionClassif$new(task, response, prob)
    }
  )
)

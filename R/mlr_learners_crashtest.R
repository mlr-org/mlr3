#' @include LearnerClassif.R
LearnerClassifCrashtest = R6Class("LearnerClassifCrashtest", inherit = LearnerClassif,
  public = list(
    initialize = function(id = "classif.crashtest") {
      super$initialize(
        id = id,
        feature_types = c("logical", "integer", "numeric", "character", "factor", "ordered"),
        predict_types = c("response", "prob"),
        param_set = ParamSet$new(
          params = list(
            ParamFct$new("crash.on", values = c("train", "predict"), default = "train"),
            ParamFct$new("crash.mode", values = c("error", "segfault"), default = "error")
          )
        ),
        param_vals = list(crash.on = "train", crash.mode = "error"),
        properties = "missings"
      )
    },

    train = function(task, crash.on, crash.mode, ...) {
      if (crash.on == "train") {
        if (crash.mode == "error")
          stop("Error in classif.crashtest during train()")
        get("attach")( structure(list(), class = "UserDefinedDatabase")  )
      }
      structure(NA, class = "crashtest.model")
    },

    predict = function(model, task, crash.on, crash.mode, ...) {
      if (crash.on == "predict") {
        if (crash.mode == "error")
          stop("Error in classif.crashtest during train()")
        get("attach")( structure(list(), class = "UserDefinedDatabase")  )
      }
    }
  )
)

#' @include mlr_learners.R
mlr_learners$add("classif.crashtest", LearnerClassifCrashtest)

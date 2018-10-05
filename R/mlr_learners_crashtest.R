#' @include LearnerClassif.R
LearnerClassifCrashtest = R6Class("LearnerClassifCrashtest", inherit = LearnerClassif,
  public = list(
    initialize = function() {
      super$initialize(
        id = "classif.crashtest",
        feature_types = c("logical", "integer", "numeric", "character", "factor", "ordered"),
        predict_types = c("response", "prob"),
        par_set = ParamSet$new(
          params = list(
            ParamCategorical$new("crash.on", values = c("train", "predict"), default = "train"),
            ParamCategorical$new("crash.mode", values = c("error", "segfault"), default = "error")
          )
        ),
        par_vals = list(crash.on = "train", crash.mode = "error"),
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

    predict = function(task, crash.on, crash.mode, ...) {
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

#' @include LearnerClassif.R
#' @include capabilities.R
LearnerClassifCrashtest = R6Class("LearnerClassifCrashtest", inherit = LearnerClassif,
  public = list(
    initialize = function() {
      super$initialize(
        id = "classif.crashtest",
        par_set = ParamSet$new(
          params = list(
            ParamCategorical$new("crash.on", values = c("train", "predict"), default = "train")
          )
        ),
      properties = capabilities$learner_props$classif,
      )
    },

    train = function(task, crash.on, ...) {
      if (crash.on == "train") {
        get("attach")( structure(list(), class = "UserDefinedDatabase")  )
      }
      structure(NA, class = "crashtest.model")
    },

    predict = function(task, crash.on, ...) {
      if (crash.on == "predict") {
        get("attach")( structure(list(), class = "UserDefinedDatabase")  )
      }
    }
  )
)

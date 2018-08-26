#' @include mlr.learners.R
#' @include LearnerClassif.R
#' @include capabilities.R
mlr.learners$add(LearnerClassif$new(
  name = "crashtest",
  par.set = ParamSetFlat$new(
    params = list(
      ParamCategorical$new("crash.on", values = c("train", "predict"), default = "train")
    )
  ),
  par.vals = list(crash.on = "train"),
  properties = capabilities$learner.props$classif,

  train = function(task, row.ids, crash.on, ...) {
    if (crash.on == "train") {
      get("attach")( structure(list(), class = "UserDefinedDatabase")  )
    }
    structure(NA, class = "crashtest.model")
  },

  predict = function(model, task, row.ids, crash.on, ...) {
    get("attach")( structure(list(), class = "UserDefinedDatabase")  )
  }
))

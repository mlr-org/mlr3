#' @include mlr_learners.R
#' @include LearnerClassif.R
#' @include capabilities.R
mlr_learners$add(LearnerClassif$new(
  name = "crashtest",
  par_set = ParamSetFlat$new(
    params = list(
      ParamCategorical$new("crash.on", values = c("train", "predict"), default = "train")
    )
  ),
  properties = capabilities$learner_props$classif,

  train = function(task, row_ids, crash.on, ...) {
    if (crash.on == "train") {
      get("attach")( structure(list(), class = "UserDefinedDatabase")  )
    }
    structure(NA, class = "crashtest.model")
  },

  predict = function(model, task, row_ids, crash.on, ...) {
    get("attach")( structure(list(), class = "UserDefinedDatabase")  )
  }
))

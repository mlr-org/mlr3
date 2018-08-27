#' @include mlr_learners.R
#' @include LearnerClassif.R
mlr_learners$add(LearnerClassif$new(
  name = "rpart",
  package = "rpart",
  par_set = ParamSetFlat$new(params = list(
    ParamInt$new(id = "minsplit", default = 20L, lower = 1L),
    ParamReal$new(id = "cp", default = 0.01, lower = 0, upper = 1),
    ParamInt$new(id = "maxcompete", default = 4L, lower = 0L),
    ParamInt$new(id = "maxsurrogate", default = 5L, lower = 0L),
    ParamInt$new(id = "maxdepth", default = 30L, lower = 1L, upper = 30L),
    ParamInt$new(id = "xval", default = 10L, lower = 0L)
  )),
  # TODO: support weights
  properties = c("twoclass", "multiclass", "missings", "feat.numeric", "feat.factor", "feat.ordered", "prob"),

  train = function(task, row_ids, ...) {
    data = task$data(row_ids)
    rpart::rpart(task$formula, data, ...)
  },

  predict = function(model, task, row_ids, ...) {
    pt = self$predict_type
    newdata = task$data(row_ids, cols = task$feature_names)
    if (pt == "response")
      as.character(predict(model, newdata = newdata, type = "class", ...))
    else
      predict(model, newdata = newdata, type = "prob", ...)
  }
))

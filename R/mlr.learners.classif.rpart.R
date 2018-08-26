#' @include mlr.learners.R
#' @include LearnerClassif.R
mlr.learners$add(LearnerClassif$new(
  name = "rpart",
  package = "rpart",
  par.set = ParamSetFlat$new(params = list(
    ParamInt$new(id = "minsplit", default = 20L, lower = 1L),
    ParamReal$new(id = "cp", default = 0.01, lower = 0, upper = 1),
    ParamInt$new(id = "maxcompete", default = 4L, lower = 0L),
    ParamInt$new(id = "maxsurrogate", default = 5L, lower = 0L),
    ParamInt$new(id = "maxdepth", default = 30L, lower = 1L, upper = 30L),
    ParamInt$new(id = "xval", default = 10L, lower = 0L)
  )),
  par.vals = list(),
  # TODO: support weights
  properties = c("twoclass", "multiclass", "missings", "feat.numeric", "feat.factor", "feat.ordered", "prob"),

  train = function(task, row.ids, ...) {
    data = task$data(row.ids)
    rpart::rpart(task$formula, data, ...)
  },

  predict = function(model, task, row.ids, ...) {
    pt = self$predict.type
    newdata = task$data(row.ids, cols = task$features)
    if (pt == "response")
      as.character(predict(model, newdata = newdata, type = "class", ...))
    else
      predict(model, newdata = newdata, type = "prob", ...)
  }
))

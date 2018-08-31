#' @include LearnerRegr.R
LearnerRegrRpart = R6Class("LearnerRegrRpart", inherit = LearnerRegr,
  public = list(
    id = "regr.rpart",
    name = "rpart",
    task_type = "TaskRegr",
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
    properties = c("missings", "feat.numeric", "feat.factor", "feat.ordered"),

    train = function(task, row_ids, ...) {
      data = task$data(row_ids)
      rpart::rpart(task$formula, data, ...)
    },

    predict = function(model, task, row_ids, ...) {
      newdata = task$data(row_ids, cols = task$feature_names)
      predict(model, newdata = newdata, ...)
    }
  )
)

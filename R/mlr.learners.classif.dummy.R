#' @include mlr.learners.R
#' @include LearnerClassif.R
mlr.learners$add(LearnerClassif$new(
  name = "dummy",
  par.set = ParamSetFlat$new(
    params = list(
      ParamCategorical$new("method", values = c("mode", "sample"), default = "mode")
    )
  ),
  par.vals = list(),
  properties = c("missings", "feat.factor", "feat.numeric"),

  train = function(task, row.ids, ...) {
    data = task$data(row.ids)
    tn = task$target
    mod = data[, .N, by = tn]
    class(mod) = c("dummy.model", class(mod))
    mod
  },

  predict = function(model, task, row.ids, method = "mode", ...) {
    if (method == "mode")
      rep.int(as.character(sample(model[N == max(N)][[task$target]], 1L)), length(row.ids))
    else
      as.character(sample(model[[task$target]], length(row.ids), replace = TRUE, prob = model[["N"]]))
  }
))

#' @include mlr_learners.R
#' @include LearnerClassif.R
mlr_learners$add(LearnerClassif$new(
  name = "dummy",
  par_set = ParamSetFlat$new(
    params = list(
      ParamCategorical$new("method", values = c("mode", "sample"), default = "mode")
    )
  ),
  properties = c("missings", "feat.factor", "feat.numeric"),

  train = function(task, row_ids, ...) {
    data = task$data(row_ids)
    tn = task$target_names
    mod = data[, .N, by = tn]
    class(mod) = c("dummy.model", class(mod))
    mod
  },

  predict = function(model, task, row_ids, method = "mode", ...) {
    if (method == "mode")
      rep.int(as.character(sample(model[N == max(N)][[task$target_names]], 1L)), length(row_ids))
    else
      as.character(sample(model[[task$target_names]], length(row_ids), replace = TRUE, prob = model[["N"]]))
  }
))

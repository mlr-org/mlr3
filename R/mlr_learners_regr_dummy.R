#' @include mlr_learners.R
#' @include LearnerRegr.R
mlr_learners$add(LearnerRegr$new(
  name = "dummy",
  par_set = ParamSetFlat$new(
    params = list(
      ParamCategorical$new("method", values = c("mean", "median"), default = "mean")
    )
  ),
  properties = c("missings", "feat.factor", "feat.numeric"),
  train = function(task, row_ids, method = "mean", ...) {
    tn = unlist(task$data(row_ids, task$target_names))
    mod = switch(method,
      "mean" = mean(tn),
      "median" = median(tn),
      stop("Illegal value for 'method'"))
    class(mod) = c("dummy.model", class(mod))
    mod
  },

  predict = function(model, task, row_ids, ...) {
    rep(as.numeric(model$model), length(row_ids))
  }
))


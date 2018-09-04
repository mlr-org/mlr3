#' @include LearnerRegr.R
LearnerRegrDummy = R6Class("LearnerRegrDummy", inherit = LearnerRegr,
  public = list(
    initialize = function() {
      super$initialize(
        id = "regr.dummy",
        par_set = ParamSet$new(
          params = list(
            ParamCategorical$new("method", values = c("mean", "median"), default = "mean")
          )
        ),
        properties = c("missings", "feat.factor", "feat.numeric"),
      )
    },

    train = function(task, method = "mean", ...) {
      tn = unlist(task$data(task$target_names))
      mod = switch(method,
        "mean" = mean(tn),
        "median" = median(tn),
        stopf("Illegal value for 'method'"))
      class(mod) = c("dummy.model", class(mod))
      mod
    },

    predict = function(task, ...) {
      n = task$nrow
      rep(as.numeric(self$model), n)
    }
  )
)

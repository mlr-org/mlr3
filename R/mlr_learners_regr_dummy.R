#' @include LearnerRegr.R
LearnerRegrDummy = R6Class("LearnerRegrDummy", inherit = LearnerRegr,
  public = list(
    initialize = function() {
      super$initialize(
        id = "regr.dummy",
        par_set = ParamSet$new(
          params = list(
            ParamFlag$new("robust", default = TRUE)
          )
        ),
        properties = c("se", "missings", "feat.factor", "feat.numeric"),
      )
    },

    train = function(task, robust = TRUE, ...) {
      tn = unlist(task$data(cols = task$target_names))
      mod = if (isTRUE(robust)) c(mean(tn), sd(tn)) else c(median(tn), madn(tn))
      class(mod) = "dummy.model"
      mod
    },

    predict = function(model, task, ...) {
      n = task$nrow
      PredictionRegr$new(
        task = task,
        response = rep(model[1L], n),
        se = rep(model[2L], n)
      )
    }
  )
)

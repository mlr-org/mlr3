#' @include LearnerClassif.R
LearnerClassifDummy = R6Class("LearnerClassifDummy", inherit = LearnerClassif,
  public = list(
    initialize = function() {
      super$initialize(
        id = "classif.dummy",
        par_set = ParamSet$new(
          params = list(
            ParamCategorical$new("method", values = c("mode", "sample"), default = "mode")
          )
        ),
        properties = c("missings", "feat.factor", "feat.numeric"),
      )
    },

    train = function(task, ...) {
      tn = task$target_names
      data = task$data(cols = tn)
      mod = data[, .N, by = tn]
      class(mod) = c("dummy.model", class(mod))
      mod
    },

    predict = function(model, task, method = "mode", ...) {
      n = task$nrow
      response = if (method == "mode") {
        rep.int(as.character(sample(model[N == max(N)][[task$target_names]], 1L)), n)
      } else {
        as.character(sample(model[[task$target_names]], n, replace = TRUE, prob = model[["N"]]))
      }

      PredictionClassif$new(task, response = response)
    }
  )
)

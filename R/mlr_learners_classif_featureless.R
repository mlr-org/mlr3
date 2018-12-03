#' @include LearnerClassif.R
LearnerClassifDummy = R6Class("LearnerClassifDummy", inherit = LearnerClassif,
  public = list(
    initialize = function(id = "classif.featureless") {
      super$initialize(
        id = id,
        feature_types = c("logical", "integer", "numeric", "character", "factor", "ordered"),
        predict_types = c("response", "prob"),
        param_set = ParamSet$new(
          params = list(
            ParamCateg$new("method", values = c("mode", "sample", "weighted.sample"), default = "mode")
          )
        ),
        properties = "missings",
      )
    },

    train = function(task, ...) {
      tn = task$target_names
      model = table(task$data(cols = tn))
      class(model) = "featureless"
      model
    },

    predict = function(model, task, method = "mode", ...) {
      n = task$nrow
      response = prob = NULL

      if (self$predict_type == "response") {
        response = switch(method,
          mode = rep.int(sample(names(model[model == max(model)]), 1L), n),
          sample = sample(names(model), n, replace = TRUE),
          weighted.sample = sample(names(model), n, replace = TRUE, prob = model)
        )
      } else if (self$predict_type == "prob") {
        prob = switch(method,
          mode = { tmp = (model == max(model)); tmp / sum(tmp) },
          sample = rep.int(1 / length(model), length(model)),
          weighted.sample = model / sum(model)
        )
        prob = matrix(prob, nrow = n, ncol = length(model), byrow = TRUE)
        colnames(prob) = names(model)
      }

      PredictionClassif$new(task, response = response, prob = prob)
    }
  )
)

#' @include mlr_learners.R
mlr_learners$add("classif.featureless", LearnerClassifDummy)

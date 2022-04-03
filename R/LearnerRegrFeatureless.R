#' @title Featureless Regression Learner
#'
#' @name mlr_learners_regr.featureless
#' @include LearnerRegr.R
#'
#' @description
#' A simple [LearnerRegr] which only analyzes the response during train, ignoring all features.
#' If hyperparameter `robust` is `FALSE` (default), constantly predicts `mean(y)` as response
#' and `sd(y)` as standard error.
#' If `robust` is `TRUE`, [median()] and [mad()] are used instead of [mean()] and [sd()],
#' respectively.
#'
#' @templateVar id regr.featureless
#' @template learner
#'
#' @template seealso_learner
#' @export
LearnerRegrFeatureless = R6Class("LearnerRegrFeatureless", inherit = LearnerRegr,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      ps = ps(
        robust = p_lgl(default = FALSE, tags = "train")
      )
      ps$values = list(robust = FALSE)

      super$initialize(
        id = "regr.featureless",
        feature_types = unname(mlr_reflections$task_feature_types),
        predict_types = c("response", "se"),
        param_set = ps,
        properties = c("featureless", "missings", "importance", "selected_features", "validation"),
        packages = "stats",
        label = "Featureless Regression Learner",
        man = "mlr3::mlr_learners_regr.featureless"
      )
    },


    #' @description
    #' All features have a score of `0` for this learner.
    #' @return Named `numeric()`.
    importance = function() {
      if (is.null(self$model)) {
        stopf("No model stored")
      }
      fn = self$model$features
      named_vector(fn, 0)
    },

    #' @description
    #' Selected features are always the empty set for this learner.
    #' @return `character(0)`.
    selected_features = function() {
      character()
    }
  ),

  private = list(
    .train = function(task, valid_ids) {
      pv = self$param_set$get_values(tags = "train")
      x_train = task$data(cols = task$target_names)[[1L]]
      x_val = task$data(rows = valid_ids, cols = task$target_names)[[1L]]
      if (isFALSE(pv$robust)) {
        location = mean(x_train)
        dispersion = sd(x_train)
        valid_error = mean((x_val - location)^2)
      } else {
        location = stats::median(x_train)
        dispersion = stats::mad(x_train, center = location)
        valid_error = stats::mad(x_val, center = location)
      }
      set_class("regr.featureless_model",
        x = list(
          location = location,
          dispersion = dispersion,
          features = task$feature_names,
          valid_error = valid_error
        )
      )
    },

    .predict = function(task) {
      n = task$nrow
      response = rep(self$model$location, n)
      se = if (self$predict_type == "se") rep(self$model$dispersion, n) else NULL
      list(response = response, se = se)
    }
  )
)

#' @include mlr_learners.R
mlr_learners$add("regr.featureless", function() LearnerRegrFeatureless$new())

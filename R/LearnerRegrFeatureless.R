#' @title Featureless Regression Learner
#'
#' @name mlr_learners_regr.featureless
#' @include LearnerRegr.R
#'
#' @description
#' A simple [LearnerRegr] which only analyses the response during train, ignoring all features.
#' If hyperparameter `robust` is `FALSE` (default), constantly predicts `mean(y)` as response
#' and `sd(y)` as standard error.
#' If `robust` is `TRUE`, [median()] and [mad()] are used instead of [mean()] and [sd()],
#' respectively.
#'
#' @templateVar id regr.featureless
#' @template section_dictionary_learner
#'
#' @section Meta Information:
#' `r rd_info(lrn("regr.featureless"))`
#'
#' @template seealso_learner
#' @export
LearnerRegrFeatureless = R6Class("LearnerRegrFeatureless", inherit = LearnerRegr,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      ps = ParamSet$new(list(
        ParamLgl$new("robust", default = TRUE, tags = "train")
      ))
      ps$values = list(robust = FALSE)

      super$initialize(
        id = "regr.featureless",
        feature_types = c("logical", "integer", "numeric", "character", "factor", "ordered"),
        predict_types = c("response", "se"),
        param_set = ps,
        properties = c("missings", "importance", "selected_features"),
        packages = "stats",
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
    .train = function(task) {
      pv = self$param_set$get_values(tags = "train")
      x = task$data(cols = task$target_names)[[1L]]
      if (isFALSE(pv$robust)) {
        location = mean(x)
        dispersion = sd(x)
      } else {
        location = stats::median(x)
        dispersion = stats::mad(x, center = location)
      }
      set_class(list(location = location, dispersion = dispersion, features = task$feature_names), "regr.featureless_model")
    },

    .predict = function(task) {
      n = task$nrow
      response = rep(self$model$location, n)
      se = if (self$predict_type == "se") rep(self$model$dispersion, n) else NULL
      PredictionRegr$new(task = task, response = response, se = se)
    }
  )
)

#' @include mlr_learners.R
mlr_learners$add("regr.featureless", LearnerRegrFeatureless)

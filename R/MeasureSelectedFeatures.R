#' @title Selected Features Measure
#'
#' @name mlr_measures_selected_features
#' @include Measure.R
#'
#' @description
#' Measures the number of selected features by extracting it from learners with property `"selected_features"`.
#' If parameter `normalize` is set to `TRUE`, the relative number of features instead of the absolute
#' number of features is returned.
#' Note that the models must be stored to be able to extract this information.
#' If the learner does not support the extraction of used features, `NA` is returned.
#'
#' This measure requires the [Task] and the [Learner] for scoring.
#'
#' @templateVar id selected_features
#' @template measure
#'
#' @template seealso_measure
#' @export
#' @examples
#' task = tsk("german_credit")
#' learner = lrn("classif.rpart")
#' rr = resample(task, learner, rsmp("cv", folds = 3), store_models = TRUE)
#'
#' scores = rr$score(msr("selected_features"))
#' scores[, c("iteration", "selected_features")]
MeasureSelectedFeatures = R6Class("MeasureSelectedFeatures",
  inherit = Measure,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      param_set = ps(normalize = p_lgl(tags = "required"))
      param_set$set_values(normalize = FALSE)

      super$initialize(
        id = "selected_features",
        param_set = param_set,
        task_type = NA_character_,
        properties = c("requires_task", "requires_learner", "requires_model", "requires_no_prediction"),
        predict_sets = NULL,
        predict_type = NA_character_,
        range = c(0, Inf),
        minimize = TRUE,
        label = "Absolute or Relative Frequency of Selected Features",
        man = "mlr3::mlr_measures_selected_features"
      )
    }
  ),

  private = list(
    .score = function(prediction, task, learner, ...) {
      learner = learner$base_learner()
      if ("selected_features" %nin% learner$properties) {
        return(NA_integer_)
      }

      n = length(learner$selected_features())
      if (self$param_set$get_values()$normalize) {
        n = n / length(task$feature_names)
      }

      return(n)
    }
  )
)

#' @include mlr_measures.R
mlr_measures$add("selected_features", function() MeasureSelectedFeatures$new())

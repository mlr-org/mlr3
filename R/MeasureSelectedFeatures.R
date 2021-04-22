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
#' @template section_dictionary_measure
#'
#' @section Parameters:
#' `r rd_info(msr("classif.costs")$param_set)`
#'
#'
#' @section Meta Information:
#' * Type: `NA`
#' * Range: \eqn{[0, \infty)}{[0, Inf)}
#' * Minimize: `TRUE`
#' * Required prediction: 'response'
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
      param_set = ps(normalize = p_lgl(default = FALSE, tags = "required"))
      param_set$values = list(normalize = FALSE)

      super$initialize(
        id = "selected_features",
        param_set = param_set,
        task_type = NA_character_,
        properties = c("requires_task", "requires_learner", "requires_model"),
        predict_type = "response",
        range = c(0, Inf),
        minimize = TRUE,
        man = "mlr3::mlr_measures_selected_features"
      )
    }
  ),

  private = list(
    .score = function(prediction, task, learner, ...) {
      if ("selected_features" %nin% learner$properties) {
        return(NA_integer_)
      }
      n = length(learner$selected_features())
      if (self$param_set$get_values()$normalize) {
        n = n / length(task$feature_names)
      }
      n
    }
  )
)

#' @include mlr_measures.R
mlr_measures$add("selected_features", MeasureSelectedFeatures)

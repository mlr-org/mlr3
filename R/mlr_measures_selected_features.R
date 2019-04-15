#' @title Selected Features Measure
#'
#' @name mlr_measures_selected_features
#' @format [R6::R6Class()] inheriting from [Measure].
#' @include Measure.R
#'
#' @description
#' Measures the number of selected features.
#' If field `normalize` is set to `TRUE`, divides the number of features by the total number of available features.
#'
#' @export
MeasureSelectedFeatures = R6Class("MeasureSelectedFeatures",
  inherit = Measure,
  cloneable = FALSE,
  public = list(
    normalize = NULL,

    initialize = function(normalize = FALSE) {
      super$initialize(
        id = "selected_features",
        task_type = NA_character_,
        predict_type = "response",
        range = c(0, Inf),
        minimize = TRUE
      )
      self$normalize = assert_flag(normalize)
    },

    calculate = function(e) {
      lrn = e$learner
      if ("selected_features" %nin% lrn$properties)
        return(NA_integer_)
      n = length(lrn$selected_features())
      if (self$normalize)
        n = n / length(e$task$feature_names)
      n
    }
  )
)

#' @title Debug Measure
#'
#' @name mlr_measures_debug
#' @include Measure.R
#'
#' @description
#' This measure returns the number of observations in the [Prediction] object.
#' Its main purpose is debugging.
#' The parameter `na_ratio` (`numeric(1)`) controls the ratio of scores which randomly
#' are set to `NA`, between 0 (default) and 1.
#'
#' @templateVar id debug
#' @template measure
#'
#' @template seealso_measure
#' @export
#' @examples
#' task = tsk("wine")
#' learner = lrn("classif.featureless")
#' measure = msr("debug", na_ratio = 0.5)
#' rr = resample(task, learner, rsmp("cv", folds = 5))
#' rr$score(measure)
MeasureDebug = R6Class("MeasureDebug",
  inherit = Measure,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      param_set = ps(na_ratio = p_dbl(0, 1, tags = "required"))
      param_set$values = list(na_ratio = 0)
      super$initialize(
        id = "debug",
        param_set = param_set,
        predict_type = "response",
        range = c(0, Inf),
        properties = "na_score",
        label = "Debug Classification Measure",
        man = "mlr3::mlr_measures_debug"
      )
    }
  ),

  private = list(
    .score = function(prediction, ...) {
      na_ratio = self$param_set$get_values()$na_ratio
      if (na_ratio > runif(1L)) {
        return(NA_integer_)
      }
      length(prediction$row_ids)
    }
  )
)

#' @include mlr_measures.R
mlr_measures$add("debug", MeasureDebug)

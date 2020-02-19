#' @title Debug Measure
#'
#' @name mlr_measures_debug
#' @include Measure.R
#'
#' @description
#' This measure returns the number of observations in the [Prediction] object.
#' Its main purpose is debugging.
#'
#' @templateVar id debug
#' @template section_dictionary_measure
#'
#' @section Meta Information:
#' * Type: `NA`
#' * Range: \eqn{[0, \infty)}{[0, Inf)}
#' * Minimize: `NA`
#' * Required prediction: 'response'
#'
#' @template seealso_measure
#' @export
#' @examples
#' task = tsk("wine")
#' learner = lrn("classif.featureless")
#' measure = msr("debug")
#' rr = resample(task, learner, rsmp("cv", folds = 3))
#' rr$score(measure)
MeasureDebug = R6Class("MeasureDebug",
  inherit = Measure,
  public = list(
    #' @field na_ratio (`numeric(1)`)\cr
    #' Ratio of scores which should be `NA`.
    na_ratio = 0,

    #' @description
    #' Creates a new instance of the [R6][R6::R6Class] object.
    #'
    #' @param na_ratio (`numeric(1)`)\cr
    #'   Matrix of costs (truth in columns, predicted response in rows).
    initialize = function(na_ratio = 0) {
      super$initialize(
        id = "debug",
        predict_type = "response",
        range = c(0, Inf),
        properties = "na_score",
        man = "mlr3::mlr_measures_debug"
      )
      self$na_ratio = assert_number(na_ratio, lower = 0, upper = 1)
    }
  ),

  private = list(
    .score = function(prediction, ...) {
      if (self$na_ratio > runif(1L))
        return(NA_integer_)
      length(prediction$row_ids)
    }
  )
)

#' @include mlr_measures.R
mlr_measures$add("debug", MeasureDebug)

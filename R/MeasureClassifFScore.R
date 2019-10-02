#' @title F-score Classification Measure
#'
#' @usage NULL
#' @aliases mlr_measures_classif.f_score
#' @format [R6::R6Class()] inheriting from [MeasureClassif].
#' @include MeasureClassif.R
#'
#' @section Construction:
#' ```
#' MeasureClassifFScore$new(beta = 1)
#' mlr_measures$get("classif.f_score")
#' msr("classif.f_score")
#' ```
#'
#' * `beta` :: `numeric(1)`\cr
#'   Non-negative parameter balancing precision and recall.
#'   Passed down to [Metrics::fbeta_score()]
#'
#' @description
#' Calls [Metrics::fbeta_score()].
#' Argument `beta` defaults to `1`.
#'
#' @references
#' Sasaki, Yutaka (2007).
#' The truth of the F-measure.
#' Teach Tutor mater 1.5: 1-5.
#' \url{https://www.cs.odu.edu/~mukka/cs795sum09dm/Lecturenotes/Day3/F-measure-YS-26Oct07.pdf}
#'
#' @template seealso_measure
#' @export
MeasureClassifFScore = R6Class("MeasureClassifFScore",
  inherit = MeasureClassif,
  public = list(
    beta = NULL,

    initialize = function(beta = 1) {
      super$initialize(
        id = "classif.f_score",
        range = 0:1,
        minimize = FALSE,
        predict_type = "response",
        task_properties = "twoclass",
        packages = "Metrics"
      )

      self$beta = assert_number(beta, lower = 0)
    },

    score_internal = function(prediction, ...) {
      truth = prediction$truth
      positive = levels(truth)[1L]
      Metrics::fbeta_score(
        actual = as.integer(truth == positive),
        predicted = as.integer(prediction$response == positive),
        beta = self$beta
      )
    }
  )
)

#' @include mlr_measures.R
mlr_measures$add("classif.f_score", MeasureClassifFScore)

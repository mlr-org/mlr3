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
#' \cite{mlr3}{sasaki_2007}
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

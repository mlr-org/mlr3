#' @title Positive Predictive Value Classification Measure
#'
#' @name mlr_measures_classif.ppv
#' @format [R6::R6Class()] inheriting from [MeasureClassif].
#' @include MeasureClassif.R
#'
#' @description
#' Defined as: TP / (TP + FP).
#' Also called precision.
#'
#' @aliases mlr_measures_classif.precision
#' @export
MeasureClassifPPV = R6Class("MeasureClassifPPV",
  inherit = MeasureClassif,
  cloneable = FALSE,
  public = list(
    initialize = function() {
      super$initialize(
        id = "classif.ppv",
        range = 0:1,
        minimize = FALSE,
        predict_type = "prob",
        task_properties = "twoclass"
      )
    },

    calculate = function(e) {
      conf = e$prediction$confusion
      denominator = sum(conf[1L, ])
      if (denominator == 0L)
        return(NA_real_)
      conf[1L, 1L] / denominator
    }
  )
)


#' @include mlr_measures.R
mlr_measures$add("classif.ppv", MeasureClassifPPV)

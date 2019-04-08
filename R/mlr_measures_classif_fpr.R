#' @title False Positive Rate Classification Measure
#'
#' @name mlr_measures_classif.fpr
#' @format [R6::R6Class()] inheriting from [MeasureClassif].
#' @include MeasureClassif.R
#'
#' @description
#' Defined as: FP / (FP + TN).
#' Also called fall-out.
#'
#' @aliases mlr_measures_classif.fallout
#' @export
MeasureClassifFPR = R6Class("MeasureClassifFPR",
  inherit = MeasureClassif,
  cloneable = FALSE,
  public = list(
    initialize = function() {
      super$initialize(
        id = "classif.fpr",
        range = 0:1,
        minimize = FALSE,
        predict_type = "prob",
        task_properties = "twoclass"
      )
    },

    calculate = function(e) {
      conf = e$prediction$confusion
      denominator = sum(conf[, 2L])
      if (denominator == 0L)
        return(NA_real_)
      conf[1L, 2L] / denominator
    }
  )
)


#' @include mlr_measures.R
mlr_measures$add("classif.fpr", MeasureClassifFPR)

#' @title True Positive Rate Classification Measure
#'
#' @name mlr_measures_classif.tpr
#' @format [R6::R6Class()] inheriting from [MeasureClassif].
#' @include MeasureClassif.R
#'
#' @description
#' Defined as: TP / (TP + FN).
#' Also called recall or sensitivity.
#'
#' @aliases mlr_measures_classif.recall
#' @aliases mlr_measures_classif.sensitivity
#' @export
MeasureClassifTPR = R6Class("MeasureClassifTPR",
  inherit = MeasureClassif,
  cloneable = FALSE,
  public = list(
    initialize = function() {
      super$initialize(
        id = "classif.tpr",
        range = 0:1,
        minimize = FALSE,
        predict_type = "prob",
        task_properties = "twoclass"
      )
    },

    calculate = function(e) {
      conf = e$prediction$p$confusion
      denominator = sum(conf[, 1L])
      if (denominator == 0L)
        return(NA_real_)
      conf[1L, 1L] / denominator
    }
  )
)


#' @include mlr_measures.R
mlr_measures$add("classif.tpr", MeasureClassifTPR)

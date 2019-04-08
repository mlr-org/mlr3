#' @title Binary Classification Measures Derived from a Confusion Matrix
#'
#' @name mlr_measures_classif.confusion
#' @format [R6::R6Class()] inheriting from [MeasureClassif].
#' @include MeasureClassif.R
#'
#' @description
#' Based on a confusion matrix for binary classification problems, allows to calculate various performance measures.
#'
#' @aliases mlr_measures_classif.confusion
#' @export
MeasureClassifConfusion = R6Class("MeasureClassifConfusion",
  inherit = MeasureClassif,
  cloneable = FALSE,
  public = list(
    initialize = function(type = "f1") {
      types = c("tp", "fn", "fp", "tn")
      super$initialize(
        id = "classif.confusion",
        range = 0:1,
        minimize = FALSE,
        predict_type = "prob",
        task_properties = "twoclass"
      )
    },

    calculate = function(e) {
    }
  )
)


confusion = function(conf, type) {
  div = function(nominator, denominator) {
    if (denominator == 0L)
      return(NA_real_)
    nominator / denominator
  }

  switch(type,
    tp = conf[1L, 1L],
    fn = conf[2L, 1L],
    fp = conf[1L, 2L],
    tn = conf[2L, 2L],
    tpr = , recall = , sensitivity = div(conf[1L, 1L], sum(conf[, 1L])),
    fnr = div(conf[2L, 1L], sum(conf[, 1L])),
    fpr = div(conf[1L, 2L], sum(conf[, 2L])),
    tnr = , specifity = div(conf[2L, 2L], sum(conf[, 2L])),
    ppv = , precision = div(conf[1L, 1L], sum(conf[1L, ])),
    fdr = div(conf[1L, 2L], sum(conf[1L, ])),
    fomr = div(conf[2L, 1L], sum(conf[2L, ])),
    npv = div(conf[2L, 2L], sum(conf[2L, ])),
    stop("Unknown method")
  )
}

mlr_measures$add("tp", function() MeasureClassifConfusion$new("tp"))

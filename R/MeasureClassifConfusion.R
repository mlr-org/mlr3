confusion_measure_info = setindexv(rowwise_table(
  ~id,           ~lower, ~upper, ~minimize, ~na_score,
  "tp",          0,      Inf,    FALSE,     FALSE,
  "fn",          0,      Inf,    TRUE,      FALSE,
  "fp",          0,      Inf,    TRUE,      FALSE,
  "tn",          0,      Inf,    FALSE,     FALSE,
  "tpr",         0,      1,      FALSE,     TRUE,
  "fnr",         0,      1,      TRUE,      TRUE,
  "fpr",         0,      1,      TRUE,      TRUE,
  "tnr",         0,      1,      FALSE,     TRUE,
  "ppv",         0,      1,      FALSE,     TRUE,
  "fdr",         0,      1,      TRUE,      TRUE,
  "for",         0,      1,      TRUE,      TRUE,
  "npv",         0,      1,      FALSE,     TRUE,
  "dor",         0,      Inf,    FALSE,     TRUE,
  "f1",          0,      1,      FALSE,     TRUE,
  "precision",   0,      1,      FALSE,     TRUE,
  "recall",      0,      1,      FALSE,     TRUE,
  "sensitivity", 0,      1,      FALSE,     TRUE,
  "specificity", 0,      1,      FALSE,     TRUE
), "id")

#' @title Binary Classification Measures Derived from a Confusion Matrix
#'
#' @usage NULL
#' @aliases mlr_measures_classif.confusion
#'  mlr_measures_classif.tp
#'  mlr_measures_classif.fn
#'  mlr_measures_classif.fp
#'  mlr_measures_classif.tn
#'  mlr_measures_classif.tpr
#'  mlr_measures_classif.fnr
#'  mlr_measures_classif.fpr
#'  mlr_measures_classif.tnr
#'  mlr_measures_classif.ppv
#'  mlr_measures_classif.fdr
#'  mlr_measures_classif.for
#'  mlr_measures_classif.npv
#'  mlr_measures_classif.dor
#'  mlr_measures_classif.precision
#'  mlr_measures_classif.recall
#'  mlr_measures_classif.sensitivity
#'  mlr_measures_classif.specificity
#' @format [R6::R6Class()] inheriting from [MeasureClassif].
#' @include MeasureClassif.R
#'
#' @section Construction:
#' ```
#' MeasureClassifConfusion$new(id = type, type)
#'
#' mlr_measures("classif.tp")
#' mlr_measures("classif.fn")
#' mlr_measures("classif.fp")
#' mlr_measures("classif.tn")
#' mlr_measures("classif.tpr")
#' mlr_measures("classif.fnr")
#' mlr_measures("classif.fpr")
#' mlr_measures("classif.tnr")
#' mlr_measures("classif.ppv")
#' mlr_measures("classif.fdr")
#' mlr_measures("classif.for")
#' mlr_measures("classif.npv")
#' mlr_measures("classif.dor")
#' mlr_measures("classif.precision")
#' mlr_measures("classif.recall")
#' mlr_measures("classif.sensitivity")
#' mlr_measures("classif.specificity")
#'
#' msr("classif.tp")
#' msr("classif.fn")
#' msr("classif.fp")
#' msr("classif.tn")
#' msr("classif.tpr")
#' msr("classif.fnr")
#' msr("classif.fpr")
#' msr("classif.tnr")
#' msr("classif.ppv")
#' msr("classif.fdr")
#' msr("classif.for")
#' msr("classif.npv")
#' msr("classif.dor")
#' msr("classif.precision")
#' msr("classif.recall")
#' msr("classif.sensitivity")
#' msr("classif.specificity")
#' ```
#'
#' * `type` :: `character(1)`\cr
#'   See [confusion_measures()].
#'
#' @description
#' All implemented [Measure]s call [confusion_measures()] with the respective `type` internally.
#' For the F1 measure, use [MeasureClassifFScore].
#'
#' @template seealso_measure
#' @export
#' @examples
#' task = tsk("german_credit")
#' learner = lrn("classif.rpart")
#' p = learner$train(task)$predict(task)
#' measures = list(msr("classif.sensitivity"), msr("classif.specificity"))
#' round(p$score(measures), 2)
MeasureClassifConfusion = R6Class("MeasureClassifConfusion",
  inherit = MeasureClassif,
  public = list(
    type = NULL,
    initialize = function(type) {
      self$type = assert_choice(type, confusion_measure_info$id)
      row = as.list(confusion_measure_info[list(type), on = "id"])

      super$initialize(
        id = sprintf("classif.%s", type),
        range = c(row$lower, row$upper),
        properties = if (row$na_score) "na_score" else character(),
        minimize = row$minimize,
        predict_type = "response",
        task_properties = "twoclass"
      )
    },

    score_internal = function(prediction, ...) {
      unname(confusion_measures(prediction$confusion, self$type))
    }
  )
)

#' @include mlr_measures.R
for (type in setdiff(confusion_measure_info$id, "f1")) {
  id = sprintf("classif.%s", type)
  mlr_measures$add(id, MeasureClassifConfusion, type = type)
}

#' @title Calculate Confusion Measures
#'
#' @description
#' Based on a 2x2 confusion matrix for binary classification problems, allows to calculate various performance measures.
#' Implemented are the following measures based on \url{https://en.wikipedia.org/wiki/Template:DiagnosticTesting_Diagram}:
#'
#' * `"tp"`: True Positives.
#' * `"fn"`: False Negatives.
#' * `"fp"`: False Positives.
#' * `"tn"`: True Negatives.
#' * `"tpr"`: True Positive Rate.
#' * `"fnr"`: False Negative Rate.
#' * `"fpr"`: False Positive Rate.
#' * `"tnr"`: True Negative Rate.
#' * `"ppv"`: Positive Predictive Value.
#' * `"fdr"`: False Discovery Rate.
#' * `"for"`: False Omission Rate.
#' * `"npv"`: Negative Predictive Value.
#' * `"dor"`: Diagnostic Odds Ratio.
#' * `"f1"`:  F1 Measure.
#' * `"precision"`: Alias for `"ppv"`.
#' * `"recall"`: Alias for `"tpr"`.
#' * `"sensitivity"`: Alias for `"tpr"`.
#' * `"specificity"`: Alias for `"tnr"`.
#'
#' If the denominator is 0, the returned score is `NA`.
#'
#' @param m :: `matrix()`\cr
#'   Confusion matrix, e.g. as returned by field `confusion` of [PredictionClassif].
#'   Truth is in columns, predicted response is in rows.
#' @param type :: `character()`\cr
#'   Selects the measure to use. See description for possible values.
#' @return (named `numeric()`) of confusion measures.
#' @export
#' @examples
#' task = tsk("german_credit")
#' learner = lrn("classif.rpart")
#' p = learner$train(task)$predict(task)
#' round(confusion_measures(p$confusion), 2)
confusion_measures = function(m, type = NULL) {
  assert_matrix(m, nrows = 2L, ncols = 2L, row.names = "unique", col.names = "unique")
  assert_names(rownames(m), identical.to = colnames(m))
  if (is.null(type)) {
    type = confusion_measure_info$id
    type = type[nchar(type) <= 3L] # filter out alias names
  } else {
    assert_subset(type, confusion_measure_info$id)
  }

  div = function(nominator, denominator) {
    if (denominator == 0L) {
      return(NA_real_)
    }
    nominator / denominator
  }

  set_names(map_dbl(type, function(type) {
    switch(type,
      "tp" = m[1L, 1L],
      "fn" = m[2L, 1L],
      "fp" = m[1L, 2L],
      "tn" = m[2L, 2L],
      "tpr" = , "recall" = , "sensitivity" = div(m[1L, 1L], sum(m[, 1L])),
      "fnr" = div(m[2L, 1L], sum(m[, 1L])),
      "fpr" = div(m[1L, 2L], sum(m[, 2L])),
      "tnr" = , "specificity" = div(m[2L, 2L], sum(m[, 2L])),
      "ppv" = , "precision" = div(m[1L, 1L], sum(m[1L, ])),
      "fdr" = div(m[1L, 2L], sum(m[1L, ])),
      "for" = div(m[2L, 1L], sum(m[2L, ])),
      "npv" = div(m[2L, 2L], sum(m[2L, ])),
      "dor" = div(m[1L, 1L] * m[2L, 2L], m[1L, 2L] * m[2L, 1L]),
      "f1"  = {
        P = div(m[1L, 1L], sum(m[1L, ]))
        R = div(m[1L, 1L], sum(m[, 1L]))
        2L * R * P / (R + P)
      }
    )
  }), type)
}

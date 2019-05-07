confusion_measure_info = setkeyv(rowwise_table(
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
  "precision",   0,      1,      FALSE,     TRUE,
  "recall",      0,      1,      FALSE,     TRUE,
  "sensitivity", 0,      1,      FALSE,     TRUE,
  "specificity", 0,      1,      FALSE,     TRUE
), "id")

#' @title Binary Classification Measures Derived from a Confusion Matrix
#'
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
#'  mlr_measures_classif.precision
#'  mlr_measures_classif.recall
#'  mlr_measures_classif.sensitivity
#'  mlr_measures_classif.specificity
#' @format [R6::R6Class()] inheriting from [MeasureClassif].
#' @include MeasureClassif.R
#'
#' @description
#' Based on a confusion matrix for binary classification problems, allows to calculate various performance measures.
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
#' * `"precision"`: Alias for `"ppv"`.
#' * `"recall"`: Alias for `"tpr"`.
#' * `"sensitivity"`: Alias for `"tpr"`.
#' * `"specificity"`: Alias for `"tnr"`.
#'
#' If the denominator is 0, the score is returned as `NA`.
#'
#' @export
#' @examples
#' task = mlr_tasks$get("wine")
#' e = Experiment$new("wine", "classif.rpart")$train()$predict()
#' m = e$prediction$confusion
#' confusion_measures(m, type = c("precision", "recall"))
MeasureClassifConfusion = R6Class("MeasureClassifConfusion",
  inherit = MeasureClassif,
  cloneable = FALSE,
  public = list(
    type = NULL,
    initialize = function(id = type, type) {
      self$type = assert_choice(type, confusion_measure_info$id)
      row = as.list(confusion_measure_info[list(type)])

      super$initialize(
        id = id,
        range = c(row$lower, row$upper),
        minimize = row$minimize,
        predict_type = "response",
        task_properties = "twoclass",
        na_score = row$na_score
      )
    },

    calculate = function(experiment = NULL, prediction = experiment$prediction) {
      unname(confusion_measures(prediction$confusion, self$type))
    }
  )
)

#' @rdname MeasureClassifConfusion
#'
#' @param m (`matrix()`)\cr
#'   Confusion matrix, e.g. as returned by field `confusion` of [PredictionClassif].
#'   Truth is in columns, predicted response is in rows.
#' @param type (`character()`)\cr
#'   Selects the measure to use. See description.
#'
#' @export
confusion_measures = function(m, type = NULL) {
  assert_matrix(m, nrows = ncol(m), row.names = "unique", col.names = "unique")
  assert_names(rownames(m), identical.to = colnames(m))
  if (is.null(type)) {
    type = confusion_measure_info$id
  } else {
    assert_subset(type, confusion_measure_info$id)
  }

  div = function(nominator, denominator) {
    if (denominator == 0L)
      return(NA_real_)
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
      "npv" = div(m[2L, 2L], sum(m[2L, ]))
    )
  }), type)
}

#' @title Calculate ROC Measures
#'
#' @description
#' Calculate a set of roc performance measures based on the confusion matrix.
#'
#' * `tpr` True positive rate (Sensitivity, Recall)
#' * `fpr` False positive rate (Fall-out)
#' * `fnr` False negative rate (Miss rate)
#' * `tnr` True negative rate (Specificity)
#' * `ppv` Positive predictive value (Precision)
#' * `fomr` False omission rate
#' * `lrp` Positive likelihood ratio (LR+)
#' * `fdr` False discovery rate
#' * `npv` Negative predictive value
#' * `acc` Accuracy
#' * `lrm` Negative likelihood ratio (LR-)
#' * `dor` Diagnostic odds ratio
#'
#' @param pred ([PredictionClassif])\cr
#' The prediction object.
#'
#' @return `list()`\cr
#'  A list containing two elements `confusion_matrix` which is the 2 times 2 confusion matrix of absolute frequencies and `measures`, a list of the above mentioned measures.
#' @export
#' @examples
#' learner = lrn("classif.rpart", predict_type = "prob")
#' splits = partition(task = tsk("pima"), ratio = 0.7)
#' task = tsk("pima")
#' learner$train(task)
#' pred = learner$predict(task)
#' score_roc_measures(pred)
score_roc_measures = function(pred) {
  assert_prediction(pred)

  score_tpr = pred$score(msr("classif.tpr"))
  score_fnr = pred$score(msr("classif.fnr"))
  score_fpr = pred$score(msr("classif.fpr"))
  score_tnr = pred$score(msr("classif.tnr"))
  score_ppv = pred$score(msr("classif.ppv"))
  score_fdr = 1 - score_ppv
  score_npv = pred$score(msr("classif.npv"))
  score_fomr = 1 - score_npv
  score_acc = pred$score(msr("classif.acc"))
  score_fpr = pred$score(msr("classif.fpr"))
  score_lr.plus = score_tpr / score_fpr
  score_lr.minus = score_fnr / score_tnr
  score_dor = score_lr.plus / score_lr.minus

  set_class(list(
    confusion_matrix = pred$confusion,
    measures = list(
      tpr = score_tpr,
      fpr = score_fpr,
      fnr = score_fnr,
      tnr = score_tnr,
      ppv = score_ppv,
      fdr = score_fdr,
      npv = score_npv,
      fomr = score_fomr,
      acc = score_acc,
      lr_plus = score_lr.plus,
      lr_minus = score_lr.minus,
      dor = score_dor)
  ), "roc_measures")
}

#' @title Print ROC Measures
#'
#' @description
#' Print the confusion matrix and a set of roc performance measures.
#'
#' @param abbreviations (`logical(1)`)\cr
#'  If `TRUE`, print a list of abbreviations for the measures.
#' @param digits (`integer(1)`)\cr
#'  Number of digits to round the measures to.
#' @export
print.roc_measures = function(x, abbreviations = TRUE, digits = 2, ...) {

  assert_flag(abbreviations)
  assert_int(digits, lower = 1)

  # format measures
  x$measures = mapply(function(m, v) paste0(m, ": ", round(v, digits)), names(x$measures), x$measures)

  res = cbind(round(x$confusion_matrix, digits = digits),
    c(x$measures[["tpr"]], x$measures[["fpr"]]),
    c(x$measures[["fnr"]], x$measures[["tnr"]]))
  res = rbind(res,
    c(x$measures[["ppv"]], x$measures[["fomr"]], x$measures[["lr_plus"]], x$measures[["acc"]]),
    c(x$measures[["fdr"]], x$measures[["npv"]], x$measures[["lr_minus"]], x$measures[["dor"]]))

  names(dimnames(res)) = c("true", "predicted")
  print(noquote(res))
  if (abbreviations) {
    catn("Abbreviations:")
    catn("tpr - True positive rate (Sensitivity, Recall)")
    catn("fpr - False positive rate (Fall-out)")
    catn("fnr - False negative rate (Miss rate)")
    catn("tnr - True negative rate (Specificity)")
    catn("ppv - Positive predictive value (Precision)")
    catn("fomr - False omission rate")
    catn("lrp - Positive likelihood ratio (LR+)")
    catn("fdr - False discovery rate")
    catn("npv - Negative predictive value")
    catn("acc - Accuracy")
    catn("lrm - Negative likelihood ratio (LR-)")
    catn("dor - Diagnostic odds ratio")
  }
}

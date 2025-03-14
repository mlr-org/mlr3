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
#' @return `list()`\cr
#' A list containing the confusion matrix and the calculated performance measures.
#'
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

  list(
    confusion_matrix = pred$confusion,
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
    dor = score_dor
  )
}

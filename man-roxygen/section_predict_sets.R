#' @section Predict Sets:
#' If you want to compare the performance of a learner on the training with the performance
#' on the validation set, you have to configure the [Learner] to predict on multiple sets by
#' setting the field `predict_sets` to `c("train", "validation")` (default is `"validation"`).
#' Each set yields a separate [Prediction] object during resampling.
#' In the next step, you have to configure the measures to operate on the respective Prediction object:
#' ```
#' m1 = msr("classif.ce", id = "ce.train", predict_sets = "train")
#' m2 = msr("classif.ce", id = "ce.validation", predict_sets = "validation")
#' ```
#'
#' The (list of) created measures can finally be passed to `$aggregate()` or `$score()`.
#' The set `"test"` is an deprecated alias name for the validation set.

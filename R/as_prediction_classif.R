#' @title Convert to a Classification Prediction
#'
#' @description
#' Convert object to a [PredictionClassif].
#' When converting a [data.frame()], all columns except `row_ids`, `truth`, `response`, `weights`,
#' and columns prefixed with `prob.` are stored as extra data.
#'
#' @inheritParams as_prediction
#'
#' @return [PredictionClassif].
#' @export
#' @examples
#' # create a prediction object
#' task = tsk("penguins")
#' learner = lrn("classif.rpart", predict_type = "prob")
#' learner$train(task)
#' p = learner$predict(task)
#'
#' # convert to a data.table
#' tab = as.data.table(p)
#'
#' # convert back to a Prediction
#' as_prediction_classif(tab)
#'
#' # split data.table into a list of data.tables
#' tabs = split(tab, tab$truth)
#'
#' # convert back to list of predictions
#' preds = lapply(tabs, as_prediction_classif)
#'
#' # calculate performance in each group
#' sapply(preds, function(p) p$score())
as_prediction_classif = function(x, ...) {
  UseMethod("as_prediction_classif")
}


#' @rdname as_prediction_classif
#' @export
# nolint next
as_prediction_classif.PredictionClassif = function(x, ...) {
  x
}


#' @rdname as_prediction_classif
#' @export
# nolint next
as_prediction_classif.data.frame = function(x, ...) {
  assert_names(names(x), must.include = c("row_ids", "truth", "response"))
  x = as.data.table(x)
  prob_cols = names(x)[startsWith(names(x), "prob.")]
  extra_cols = setdiff(names(x), c("row_ids", "truth", "response", "weights", prob_cols))

  prob = if (length(prob_cols)) {
    prob = as.matrix(x[, prob_cols, with = FALSE])
    colnames(prob) = substr(colnames(prob), 6L, nchar(colnames(prob)))
    prob
  }
  extra = if (length(extra_cols)) as.list(x[, extra_cols, with = FALSE])

  args = x[, setdiff(names(x), c(prob_cols, extra_cols)), with = FALSE]
  invoke(PredictionClassif$new, prob = prob, extra = extra, .args = args)
}

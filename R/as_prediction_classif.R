#' @title Convert to a Classification Prediction
#'
#' @description
#' Convert object to a [PredictionClassif].
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
as_prediction_classif.PredictionClassif = function(x, ...) { # nolint
  x
}


#' @rdname as_prediction_classif
#' @export
as_prediction_classif.data.frame = function(x, ...) { # nolint
  assert_names(names(x), must.include = c("row_ids", "truth", "response"))
  prob_cols = setdiff(names(x), c("row_ids", "truth", "response", "weights"))
  if (!all(startsWith(prob_cols, "prob."))) {
    stopf("Table may only contain columns 'row_ids', 'truth', 'response', 'weights' as well as columns prefixed with 'prob.' for class probabilities")
  }

  x = as.data.table(x)
  if (length(prob_cols)) {
    prob = as.matrix(x[, prob_cols, with = FALSE])
    colnames(prob) = substr(colnames(prob), 6L, nchar(colnames(prob)))
  } else {
    prob = NULL
  }

  invoke(PredictionClassif$new, prob = prob, .args = x[, -prob_cols, with = FALSE])
}

#' @title Convert to a Regression Prediction
#'
#' @description
#' Convert object to a [PredictionRegr].
#'
#' @inheritParams as_prediction
#'
#' @return [PredictionRegr].
#' @export
#' @examples
#' # create a prediction object
#' task = tsk("mtcars")
#' learner = lrn("regr.rpart")
#' learner$train(task)
#' p = learner$predict(task)
#'
#' # convert to a data.table
#' tab = as.data.table(p)
#'
#' # convert back to a Prediction
#' as_prediction_regr(tab)
#'
#' # split data.table into a list of data.tables
#' tabs = split(tab, cut(tab$truth, 3))
#'
#' # convert back to list of predictions
#' preds = lapply(tabs, as_prediction_regr)
#'
#' # calculate performance in each group
#' sapply(preds, function(p) p$score())
as_prediction_regr = function(x, ...) {
  UseMethod("as_prediction_regr")
}


#' @rdname as_prediction_regr
#' @export
as_prediction_regr.PredictionRegr = function(x, ...) { # nolint
  x
}


#' @rdname as_prediction_regr
#' @export
as_prediction_regr.data.frame = function(x, ...) { # nolint
  assert_names(names(x), must.include = c("row_ids", "truth", "response"))
  assert_names(names(x), subset.of = c("row_ids", "truth", "response", "se", "weights"))
  invoke(PredictionRegr$new, .args = x)
}

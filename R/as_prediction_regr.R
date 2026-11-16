#' @title Convert to a Regression Prediction
#'
#' @description
#' Convert object to a [PredictionRegr].
#' When converting a [data.frame()], columns named `q<prob>` (e.g., `q0.5`) are converted to the quantile matrix.
#' All other columns except `row_ids`, `truth`, `response`, `se`, and `weights` are stored as extra data.
#' Tables with a `distr` column cannot be converted.
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
# nolint next
as_prediction_regr.PredictionRegr = function(x, ...) {
  x
}


#' @rdname as_prediction_regr
#' @export
# nolint next
as_prediction_regr.data.frame = function(x, ...) {
  assert_names(names(x), must.include = c("row_ids", "truth", "response"))
  if ("distr" %chin% names(x)) {
    error_input("Cannot convert a table with column 'distr', distribution predictions are not supported")
  }
  x = as.data.table(x)
  quantile_cols = names(x)[is_quantile_col(names(x))]
  extra_cols = setdiff(names(x), c("row_ids", "truth", "response", "se", "weights", quantile_cols))
  drop_cols = c(quantile_cols, extra_cols)

  quantiles = if (length(quantile_cols)) {
    probs = as.numeric(substring(quantile_cols, 2L))
    quantiles = as.matrix(x[, quantile_cols, with = FALSE])
    setattr(quantiles, "probs", probs)
    # the response of a quantile prediction is one of the quantile columns
    i = which(map_lgl(quantile_cols, function(col) identical(x[[col]], x$response)))
    if (length(i)) {
      setattr(quantiles, "response", probs[i[1L]])
      drop_cols = c(drop_cols, "response")
    }
    quantiles
  }
  extra = if (length(extra_cols)) as.list(x[, extra_cols, with = FALSE])

  args = x[, setdiff(names(x), drop_cols), with = FALSE]
  invoke(PredictionRegr$new, quantiles = quantiles, extra = extra, .args = args)
}

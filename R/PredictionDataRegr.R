#' @rdname PredictionData
#' @export
check_prediction_data.PredictionDataRegr = function(pdata, ...) { # nolint
  pdata$row_ids = assert_row_ids(pdata$row_ids)
  n = length(pdata$row_ids)
  if (is.null(pdata$truth)) pdata$truth = NA_real_
  if (!length(pdata$row_ids)) pdata$truth = numeric(0)

  if (!is.null(pdata$response)) {
    pdata$response = assert_numeric(unname(pdata$response))
    assert_prediction_count(length(pdata$response), n, "response")
  }

  if (!is.null(pdata$se)) {
    pdata$se = assert_numeric(unname(pdata$se), lower = 0)
    assert_prediction_count(length(pdata$se), n, "se")
  }

  pdata
}


#' @rdname PredictionData
#' @export
is_missing_prediction_data.PredictionDataRegr = function(pdata, ...) { # nolint
  miss = logical(length(pdata$row_ids))

  if (!is.null(pdata$response)) {
    miss = is.na(pdata$response)
  }

  if (!is.null(pdata$se)) {
    miss = miss | is.na(pdata$se)
  }

  pdata$row_ids[miss]
}


#' @rdname PredictionData
#' @export
c.PredictionDataRegr = function(..., keep_duplicates = TRUE) { # nolint
  dots = list(...)
  assert_list(dots, "PredictionDataRegr")
  assert_flag(keep_duplicates)
  if (length(dots) == 1L) {
    return(dots[[1L]])
  }

  predict_types = names(mlr_reflections$learner_predict_types$regr)
  predict_types = map(dots, function(x) intersect(names(x), predict_types))
  if (!every(predict_types[-1L], setequal, y = predict_types[[1L]])) {
    stopf("Cannot combine predictions: Different predict types")
  }

  elems = c("row_ids", "truth", intersect(predict_types[[1L]], c("response", "se")))
  tab = map_dtr(dots, function(x) x[elems], .fill = FALSE)

  if (!keep_duplicates) {
    tab = unique(tab, by = "row_ids", fromLast = TRUE)
  }

  new_prediction_data(as.list(tab), "regr")
}

#' @export
filter_prediction_data.PredictionDataRegr = function(pdata, row_ids, ...) {
  keep = pdata$row_ids %in% row_ids
  pdata$row_ids = pdata$row_ids[keep]
  pdata$truth = pdata$truth[keep]

  if (!is.null(pdata$response)) {
    pdata$response = pdata$response[keep]
  }

  if (!is.null(pdata$se)) {
    pdata$se = pdata$se[keep]
  }

  pdata
}

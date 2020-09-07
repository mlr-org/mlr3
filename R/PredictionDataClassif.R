#' @export
check_prediction_data.PredictionDataClassif = function(pdata) { # nolint
  row_ids = assert_row_ids(pdata$row_ids)
  n = length(row_ids)
  assert_factor(pdata$truth, len = n, null.ok = TRUE)
  lvls = levels(pdata$truth)

  if (!is.null(pdata$response)) {
    pdata$response = assert_factor(as_factor(pdata$response, levels = lvls), len = n)
  }

  if (!is.null(pdata$prob)) {
    prob = assert_matrix(pdata$prob, nrows = n, ncols = length(lvls))
    assert_numeric(prob, lower = 0, upper = 1)
    if (!identical(colnames(prob), lvls)) {
      assert_names(colnames(prob), permutation.of = lvls)
      prob = prob[, match(colnames(prob), lvls), drop = FALSE]
    }
    if (!is.null(rownames(prob))) {
      rownames(prob) = NULL
    }
    pdata$prob = prob

    if (is.null(pdata$response)) {
      # calculate response from prob
      i = max.col(prob, ties.method = "random")
      pdata$response = factor(colnames(prob)[i], levels = lvls)
    }
  }

  pdata
}



#' @export
is_missing_prediction_data.PredictionDataClassif = function(pdata) { # nolint
  miss = logical(length(pdata$row_ids))
  if (!is.null(pdata$response)) {
    miss = is.na(pdata$response)
  }
  if (!is.null(pdata$prob)) {
    miss = miss | apply(pdata$prob, 1L, anyMissing)
  }

  pdata$row_ids[miss]
}

#' @export
c.PredictionDataClassif = function(..., keep_duplicates = TRUE) {
  dots = list(...)
  assert_list(dots, "PredictionDataClassif")
  assert_flag(keep_duplicates)
  if (length(dots) == 1L) {
    return(dots[[1L]])
  }

  predict_types = names(mlr_reflections$learner_predict_types$classif)
  predict_types = map(dots, function(x) intersect(names(x), predict_types))
  if (!every(predict_types[-1L], setequal, y = predict_types[[1L]])) {
    stopf("Cannot rbind predictions: Different predict types")
  }

  elems = c("row_ids", "truth", intersect(predict_types[[1L]], "response"))
  tab = map_dtr(dots, function(x) x[elems], .fill = FALSE)
  prob = do.call(rbind, map(dots, "prob"))

  if (!keep_duplicates) {
    keep = !duplicated(tab, by = "row_ids", fromLast = TRUE)
    tab = tab[keep]
    prob = prob[keep,, drop = FALSE]
  }

  result = as.list(tab)
  result$prob = prob
  set_class(result, "PredictionDataClassif")
}

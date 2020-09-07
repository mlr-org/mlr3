#' @export
check_prediction_data.PredictionDataRegr = function(pdata) { # nolint
  row_ids = assert_row_ids(pdata$row_ids)
  n = length(row_ids)

  assert_numeric(pdata$response, len = n, any.missing = FALSE, null.ok = TRUE)
  assert_numeric(pdata$se, len = n, lower = 0, any.missing = FALSE, null.ok = TRUE)

  if (!is.null(pdata$distr)) {
    assert_class(pdata$distr, "VectorDistribution")

    if (is.null(pdata$response)) {
      pdata$response = unname(pdata$distr$mean())
    }

    if (is.null(pdata$se)) {
      pdata$se = unname(pdata$distr$stdev())
    }
  }

  pdata
}


#' @export
is_missing_prediction_data.PredictionDataRegr = function(pdata) { # nolint
  miss = logical(length(pdata$row_ids))

  if (!is.null(pdata$response)) {
    miss = is.na(pdata$response)
  }

  if (!is.null(pdata$se)) {
    miss = miss | is.na(pdata$se)
  }

  pdata$row_ids[miss]
}


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
  tab = map_dtr(dots, `[`, elems, .fill = FALSE)

  if (!keep_duplicates) {
    tab = unique(tab, by = "row_ids", fromLast = TRUE)
  }

  result = as.list(tab)

  if ("distr" %in% predict_types[[1L]]) {
    require_namespaces("distr6")
    result$distr = do.call(c, map(dots, "distr"))
  }

  set_class(result, "PredictionDataRegr")
}

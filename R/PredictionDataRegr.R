#' @rdname PredictionData
#' @export
as_prediction.PredictionDataRegr = function(x, check = TRUE) { # nolint
  invoke(PredictionRegr$new, check = check, .args = x)
}

#' @rdname PredictionData
#' @export
check_prediction_data.PredictionDataRegr = function(pdata) { # nolint
  pdata$row_ids = assert_row_ids(pdata$row_ids)
  n = length(pdata$row_ids)

  # FIXME: any.missing = FALSE does no longer work with new changes
  assert_numeric(pdata$response, len = n, any.missing = TRUE, null.ok = TRUE)
  assert_numeric(pdata$se, len = n, lower = 0, any.missing = TRUE, null.ok = TRUE)

  if (!is.null(pdata$distr)) {
    assert_class(pdata$distr, "VectorDistribution")

    if (is.null(pdata$response)) {
      pdata$response = unname(pdata$distr$mean())
    }

    if (is.null(pdata$se)) {
      pdata$se = unname(pdata$distr$stdev())
    }
  }
  # FIXME: impact

  pdata
}


#' @rdname PredictionData
#' @export
is_missing_prediction_data.PredictionDataRegr = function(pdata) { # nolint
  miss = logical(length(pdata$row_ids))

  if (!is.null(pdata$response)) {
    miss = is.na(pdata$response)
  }

  if (!is.null(pdata$se)) {
    miss = miss | is.na(pdata$se)
  }
  # FIXME: impact

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
  impact = map(dots, "impact")
  nms = names(impact[[1L]])
  impact = pmap(impact, c)
  names(impact) = nms

  if (!keep_duplicates) {
    keep = !duplicated(tab, by = "row_ids", fromLast = TRUE)
    tab = tab[keep]
    impact = map(impact, function(x) x[keep])

  }

  result = as.list(tab)

  if ("distr" %in% predict_types[[1L]]) {
    require_namespaces("distr6")
    result$distr = do.call(c, map(dots, "distr"))
  }

  result$impact = if(length(impact) == 0L) NULL else impact

  new_prediction_data(result, "regr")
}

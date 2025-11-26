#' @rdname PredictionData
#' @export
check_prediction_data.PredictionDataRegr = function(pdata, ...) { # nolint
  pdata$row_ids = assert_row_ids(pdata$row_ids)
  n = length(pdata$row_ids)
  if (is.null(pdata$truth)) pdata$truth = NA_real_
  if (!length(pdata$row_ids)) pdata$truth = numeric()

  if (!is.null(pdata$response)) {
    pdata$response = assert_numeric(unname(pdata$response))
    assert_prediction_count(length(pdata$response), n, "response")
  }

  if (!is.null(pdata$se)) {
    pdata$se = assert_numeric(unname(pdata$se), lower = 0)
    assert_prediction_count(length(pdata$se), n, "se")
  }

  if (!is.null(pdata$quantiles)) {
    quantiles = pdata$quantiles
    assert_matrix(quantiles)
    assert_prediction_count(nrow(quantiles), n, "quantiles")

    if (is.null(attr(quantiles, "probs"))) {
      error_learner_predict("No probs attribute stored in 'quantile'")
    }

    if (is.null(attr(quantiles, "response")) && is.null(pdata$response)) {
      error_learner_predict("No response attribute stored in 'quantile' or response stored in 'pdata'")
    }

    colnames(pdata$quantiles) = sprintf("q%g", attr(quantiles, "probs"))
    setattr(pdata$quantiles, "response", sprintf("q%g", attr(quantiles, "response")))
  }

  if (!is.null(pdata$distr)) {
    assert_class(pdata$distr, "VectorDistribution")

    if (is.null(pdata$response)) {
      pdata$response = unname(pdata$distr$mean())
    }

    if (is.null(pdata$se)) {
      pdata$se = unname(pdata$distr$stdev())
    }
  }

  if (!is.null(pdata$weights)) {
    # weights may never be NA, even if no prediction was made.
    pdata$weights = assert_numeric(unname(pdata$weights), any.missing = FALSE)
    assert_prediction_count(length(pdata$weights), n, "weights")
  }

  if (!is.null(pdata$extra)) {
    assert_list(pdata$extra, names = "unique")
    len = lengths(pdata$extra)
    if (any(len != n)) {
      error_learner_predict("Extra data must have the same length as the number of predictions")
    }
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

  if (!is.null(pdata$quantiles)) {
    miss = miss | apply(pdata$quantiles, 1L, anyMissing)
  }

  # weights may never be NA, so we don't need to check for missingness

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
    error_input("Cannot combine predictions: Different predict types")
  }

  if (length(unique(map_lgl(dots, function(x) is.null(x$weights)))) > 1L) {
    error_input("Cannot combine predictions: Some predictions have weights, others do not")
  }

  if (length(unique(map_lgl(dots, function(x) is.null(x$extra)))) > 1L) {
    error_input("Cannot rbind predictions: Some predictions have extra data, others do not")
  }

  nn = names(dots[[1L]])
  elems = c("row_ids", "truth", intersect(predict_types[[1L]], c("response", "se")), if ("weights" %chin% nn) "weights")
  tab = map_dtr(dots, function(x) x[elems], .fill = FALSE)

  quantiles = if ("quantiles" %chin% nn) {
    quantiles = map(dots, "quantiles")
    attrs = attributes(quantiles[[1L]])
    quantiles = do.call(rbind, quantiles)
    setattr(quantiles, "probs", attrs$props)
    setattr(quantiles, "response", attrs$response)
  }
  extra = if ("extra" %chin% nn) {
    rbindlist(map(dots, "extra"), fill = TRUE, use.names = TRUE)
  }

  if (!keep_duplicates) {
    keep = !duplicated(tab, by = "row_ids", fromLast = TRUE)
    tab = tab[keep]
    quantiles = quantiles[keep, , drop = FALSE]
    extra = extra[keep]
  }

  result = as.list(tab)
  result$quantiles = quantiles
  if (!is.null(extra)) result$extra = as.list(extra)

  if ("distr" %chin% predict_types[[1L]]) {
    require_namespaces("distr6", msg = "To predict probability distributions, please install %s")
    result$distr = do.call(c, map(dots, "distr"))
  }

  new_prediction_data(result, "regr")
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

  if (!is.null(pdata$quantiles)) {
    pdata$quantiles = pdata$quantiles[keep, , drop = FALSE]
  }

  if (!is.null(pdata$weights)) {
    pdata$weights = pdata$weights[keep]
  }

  if (!is.null(pdata$extra)) {
    pdata$extra = map(pdata$extra, function(x) x[keep])
  }

  pdata
}

#' @export
create_empty_prediction_data.TaskRegr = function(task, learner) {
  predict_types = mlr_reflections$learner_predict_types[["regr"]][[learner$predict_type]]

  pdata = list(
    row_ids = integer(),
    truth = numeric()
  )

  if ("response" %chin% predict_types) {
    pdata$response = pdata$truth
  }

  if ("se" %chin% predict_types) {
    pdata$se = pdata$truth
  }

  if ("distr" %chin% predict_types) {
    pdata$distr = list()
  }

  if ("weights_measure" %chin% task$properties) {
    pdata$weights = numeric()
  }

  new_prediction_data(pdata, "regr")
}

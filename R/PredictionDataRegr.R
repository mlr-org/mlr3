#' @rdname PredictionData
#' @export
# nolint next
check_prediction_data.PredictionDataRegr = function(pdata, ...) {
  pdata$row_ids = assert_row_ids(pdata$row_ids)
  n = length(pdata$row_ids)
  if (is.null(pdata$truth)) {
    pdata$truth = rep(NA_real_, n)
  }
  if (!length(pdata$row_ids)) {
    pdata$truth = numeric()
  }
  assert_numeric(pdata$truth, len = n, .var.name = "truth")

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
    assert_matrix(quantiles, mode = "numeric")
    assert_prediction_count(nrow(quantiles), n, "quantiles")

    probs = attr(quantiles, "probs")
    if (is.null(probs)) {
      error_learner_predict("No probs attribute stored in 'quantile'")
    }

    if (length(probs) != ncol(quantiles)) {
      error_learner_predict(
        "Predicted quantiles have %i columns, but the probs attribute has %i elements",
        ncol(quantiles),
        length(probs)
      )
    }

    response = attr(quantiles, "response")
    if (is.null(response) && is.null(pdata$response)) {
      error_learner_predict("No response attribute stored in 'quantile' or response stored in 'pdata'")
    }

    quantile_names = sprintf("q%g", probs)
    if (anyDuplicated(quantile_names)) {
      error_learner_predict("Quantiles must be unique, but got: %s", str_collapse(quantile_names))
    }

    if (is.numeric(response)) {
      response = sprintf("q%g", response)
    }
    if (!is.null(response) && response %nin% quantile_names) {
      error_learner_predict("Quantile response '%s' is not one of the predicted quantiles", response)
    }

    colnames(pdata$quantiles) = quantile_names
    if (!is.null(response)) {
      setattr(pdata$quantiles, "response", response)
    }
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
    nn = names(pdata$extra)
    reserved = nn[nn %chin% c("row_ids", "truth", "response", "se", "distr", "weights") | is_quantile_col(nn)]
    if (length(reserved)) {
      error_input("Extra data may not use reserved prediction column names: %s", str_collapse(reserved))
    }
    len = lengths(pdata$extra)
    if (any(len != n)) {
      error_learner_predict("Extra data must have the same length as the number of predictions")
    }
  }

  pdata
}


#' @rdname PredictionData
#' @export
# nolint next
is_missing_prediction_data.PredictionDataRegr = function(pdata, ...) {
  miss = logical(length(pdata$row_ids))

  if (!is.null(pdata$response)) {
    miss = is.na(pdata$response)
  }

  if (!is.null(pdata$se)) {
    miss = miss | is.na(pdata$se)
  }

  if (!is.null(pdata$quantiles)) {
    miss = miss | row_any_na(pdata$quantiles)
  }

  # weights may never be NA, so we don't need to check for missingness

  pdata$row_ids[miss]
}


#' @rdname PredictionData
#' @export
# nolint next
c.PredictionDataRegr = function(..., keep_duplicates = TRUE) {
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
    probs = attr(dots[[1L]]$quantiles, "probs")
    response = attr(dots[[1L]]$quantiles, "response")
    same = every(dots[-1L], function(x) {
      identical(attr(x$quantiles, "probs"), probs) && identical(attr(x$quantiles, "response"), response)
    })
    if (!same) {
      error_input("Cannot combine predictions: Different quantiles")
    }
    do.call(rbind, map(dots, "quantiles"))
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
  if (!is.null(quantiles)) {
    setattr(quantiles, "probs", attr(dots[[1L]]$quantiles, "probs"))
    setattr(quantiles, "response", attr(dots[[1L]]$quantiles, "response"))
  }
  result$quantiles = quantiles
  if (!is.null(extra)) {
    result$extra = as.list(extra)
  }

  if ("distr" %chin% predict_types[[1L]]) {
    require_namespaces("distr6", msg = "To predict probability distributions, please install %s")
    result$distr = do.call(c, map(dots, "distr"))
    if (!keep_duplicates) {
      result$distr = subset_distr(result$distr, which(keep))
    }
  }

  raw = discard(map(dots, "raw"), is.null)
  if (length(raw)) {
    result$raw = raw
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
    quantiles = pdata$quantiles[keep, , drop = FALSE]
    setattr(quantiles, "probs", attr(pdata$quantiles, "probs"))
    setattr(quantiles, "response", attr(pdata$quantiles, "response"))
    pdata$quantiles = quantiles
  }

  if (!is.null(pdata$distr)) {
    pdata$distr = subset_distr(pdata$distr, which(keep))
  }

  if (!is.null(pdata$weights)) {
    pdata$weights = pdata$weights[keep]
  }

  if (!is.null(pdata$extra)) {
    pdata$extra = map(pdata$extra, function(x) x[keep])
  }

  pdata
}

# `[.VectorDistribution` errors on an empty index and drops the wrapper for a single index
subset_distr = function(distr, i) {
  if (!length(i)) {
    return(list())
  }

  if (length(i) > 1L) {
    return(distr[i])
  }

  d = distr[i]
  if (inherits(d, "VectorDistribution")) {
    return(d)
  }

  vector_distribution = getExportedValue("distr6", "VectorDistribution")
  if (inherits(d, "SDistribution")) {
    vector_distribution$new(
      distribution = class(d)[1L],
      params = list(d$parameters()$values),
      decorators = distr$decorators
    )
  } else {
    vector_distribution$new(distlist = list(d), decorators = distr$decorators)
  }
}

#' @export
create_empty_prediction_data.TaskRegr = function(task, learner) {
  predict_types = mlr_reflections$learner_predict_types[["regr"]][[learner$predict_type]]

  pdata = list(
    row_ids = integer(),
    truth = numeric()
  )

  quantile_response = if ("quantiles" %chin% predict_types) learner$quantile_response

  # the response of a quantile prediction is stored in the quantiles matrix
  if ("response" %chin% predict_types && is.null(quantile_response)) {
    pdata$response = pdata$truth
  }

  if ("se" %chin% predict_types) {
    pdata$se = pdata$truth
  }

  if ("quantiles" %chin% predict_types) {
    probs = learner$quantiles
    quantiles = matrix(numeric(), nrow = 0L, ncol = length(probs), dimnames = list(NULL, sprintf("q%g", probs)))
    setattr(quantiles, "probs", probs)
    if (!is.null(quantile_response)) {
      setattr(quantiles, "response", sprintf("q%g", quantile_response))
    }
    pdata$quantiles = quantiles
  }

  if ("distr" %chin% predict_types) {
    pdata$distr = list()
  }

  if ("weights_measure" %chin% task$properties) {
    pdata$weights = numeric()
  }

  new_prediction_data(pdata, "regr")
}

# quantile columns are named "q" followed by the probability, formatted with "%g"
is_quantile_col = function(x) {
  grepl("^q[0-9.]+(e[-+]?[0-9]+)?$", x)
}

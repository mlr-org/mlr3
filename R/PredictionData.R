#' @export
check_prediction_data = function(pdata) {
  UseMethod("check_prediction_data")
}

#' @export
check_prediction_data.PredictionDataClassif = function(pdata) { # nolint
  row_id = assert_row_ids(pdata$row_id)
  n = length(row_id)
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
check_prediction_data.PredictionDataRegr = function(pdata) { # nolint
  row_id = assert_row_ids(row_id)
  n = length(row_id)

  assert_numeric(pdata$response, len = n, any.missing = FALSE, null.ok = TRUE)
  assert_numeric(pdata$se, len = n, lower = 0, any.missing = FALSE, null.ok = TRUE)

  if (!is.null(pdata$distr)) {
    assert_class(pdata$distr, "VectorDistribution")

    if (is.null(pdata$response)) {
      pdata$response = unname(distr$mean())
    }

    if (is.null(pdata$se)) {
      pdata$se = unname(distr$stdev())
    }
  }

  pdata
}

#' @rdname PredictionData
#' @param train_task ([Task])\cr
#'   Task used for training the learner.
#' @export
check_prediction_data.PredictionDataClassif = function(pdata, train_task, ...) { # nolint
  pdata$row_ids = assert_row_ids(pdata$row_ids)
  n = length(pdata$row_ids)
  assert_factor(pdata$truth, len = n, null.ok = TRUE)
  # unsupervised task
  if (is.null(pdata$truth)) {
    lvls = fget(train_task$col_info, train_task$target_names, "levels", "id")
    pdata$truth = if (length(pdata$row_ids)) factor(NA, lvls) else factor(levels = lvls)
  }
  lvls = levels(pdata$truth)

  if (!is.null(pdata$response)) {
    pdata$response = assert_factor(as_factor(unname(pdata$response), levels = lvls))
    assert_prediction_count(length(pdata$response), n, "response")
  }

  if (!is.null(pdata$prob)) {
    prob = assert_matrix(pdata$prob)
    assert_prediction_count(nrow(pdata$prob), n, "prob")
    assert_numeric(prob, lower = 0, upper = 1)
    assert_row_sums(prob)

    if (!identical(colnames(prob), lvls)) {
      assert_subset(colnames(prob), lvls)

      # add missing columns with prob == 0
      miss = setdiff(lvls, colnames(prob))
      if (length(miss)) {
        prob = cbind(prob, matrix(0, nrow = n, ncol = length(miss), dimnames = list(NULL, miss)))
      }

      # reorder columns to match the level order of `truth`
      prob = prob[, reorder_vector(colnames(prob), lvls), drop = FALSE]
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

  if (!is.null(pdata$weights)) {
    # weights may never be NA, even if no prediction was made.
    pdata$weights = assert_numeric(unname(pdata$weights), any.missing = FALSE)
    assert_prediction_count(length(pdata$weights), n, "weights")
  }

  pdata
}



#' @rdname PredictionData
#' @export
is_missing_prediction_data.PredictionDataClassif = function(pdata, ...) { # nolint
  miss = logical(length(pdata$row_ids))
  if (!is.null(pdata$response)) {
    miss = is.na(pdata$response)
  }
  if (!is.null(pdata$prob)) {
    miss = miss | apply(pdata$prob, 1L, anyMissing)
  }

  # weights may never be NA, so we don't need to check for missingness

  pdata$row_ids[miss]
}

#' @rdname PredictionData
#'
#' @param keep_duplicates (`logical(1)`)
#'   If `TRUE`, the combined [PredictionData] object is filtered for duplicated
#'   row ids (starting from last).
#' @param ... (one or more [PredictionData] objects).
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

  if (length(unique(map_lgl(dots, function(x) is.null(x$weights)))) > 1L) {
    stopf("Cannot rbind predictions: Some predictions have weights, others do not")
  }

  elems = c("row_ids", "truth", intersect(predict_types[[1L]], "response"), if ("weights" %chin% names(dots[[1L]])) "weights")
  tab = map_dtr(dots, function(x) x[elems], .fill = FALSE)
  prob = do.call(rbind, map(dots, "prob"))

  if (!keep_duplicates) {
    keep = !duplicated(tab, by = "row_ids", fromLast = TRUE)
    tab = tab[keep]
    prob = prob[keep, , drop = FALSE]
  }

  result = as.list(tab)
  result$prob = prob
  new_prediction_data(result, "classif")
}

#' @export
filter_prediction_data.PredictionDataClassif = function(pdata, row_ids, ...) {
  keep = pdata$row_ids %in% row_ids
  pdata$row_ids = pdata$row_ids[keep]
  pdata$truth = pdata$truth[keep]

  if (!is.null(pdata$response)) {
    pdata$response = pdata$response[keep]
  }

  if (!is.null(pdata$prob)) {
    pdata$prob = pdata$prob[keep,, drop = FALSE]
  }

  if (!is.null(pdata$weights)) {
    pdata$weights = pdata$weights[keep]
  }

  pdata
}

#' @export
create_empty_prediction_data.TaskClassif = function(task, learner) {
  predict_types = mlr_reflections$learner_predict_types[["classif"]][[learner$predict_type]]
  cn = task$class_names

  pdata = list(
    row_ids = integer(),
    truth = factor(character(), levels = cn)
  )

  if ("response" %chin% predict_types) {
    pdata$response = pdata$truth
  }

  if ("prob" %chin% predict_types) {
    pdata$prob = matrix(numeric(), nrow = 0L, ncol = length(cn), dimnames = list(NULL, cn))
  }

  if ("weights_measure" %chin% task$properties) {
    pdata$weights = numeric()
  }

  return(new_prediction_data(pdata, "classif"))
}

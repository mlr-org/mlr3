new_prediction_data = function(li, task_type = NULL) {
  capitalize = function(str) {
    substr(str, 1L, 1L) = toupper(substr(str, 1L, 1L))
    str
  }

  li = discard(li, is.null)
  class(li) = if (is.null(task_type)) {
    "PredictionData"
  } else {
    sprintf("PredictionData%s", c(capitalize(task_type), ""))
  }

  li
}

#' @export
check_prediction_data = function(pdata) {
  UseMethod("check_prediction_data")
}

#' @export
print.PredictionData = function(x, ...) {
  catf("<%s:%i>", class(x)[1L], length(x$row_id))
}

## FIXME:
## remove this function as soon as all learners have been migrated
as_prediction_data = function(result, task) {
  if (inherits(result, "Prediction")) {
    result = set_names(lapply(result$predict_types, function(x) result[[x]]), result$predict_types)
  } else {
    predict_types = names(mlr_reflections$learner_predict_types[[task$task_type]])
    assert_list(result, names = "unique")
    assert_names(names(result), subset.of = predict_types)
  }

  result = insert_named(result, list(row_id = task$row_ids, truth = task$truth(task$row_ids)))
  new_prediction_data(result, task_type = task$task_type)
}

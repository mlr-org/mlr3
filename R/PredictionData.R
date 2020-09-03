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
print.PredictionData = function(x, ...) {
  catf("<%s:%i>", class(x)[1L], length(x$row_id))
}

#' @export
check_prediction_data = function(pdata) {
  UseMethod("check_prediction_data")
}

#' @export
is_missing_prediction_data = function(pdata) {
  UseMethod("is_missing_prediction_data")
}


## FIXME:
## remove this function as soon as all learners have been migrated
as_prediction_data = function(result, task, row_ids = task$row_ids) {
  if (inherits(result, "Prediction")) {
    result = set_names(lapply(result$predict_types, function(x) result[[x]]), result$predict_types)
  } else {
    predict_types = names(mlr_reflections$learner_predict_types[[task$task_type]])
    assert_list(result, names = "unique")
    assert_names(names(result), subset.of = predict_types)
  }

  result = insert_named(result, list(row_id = row_ids, truth = task$truth(row_ids)))
  new_prediction_data(result, task_type = task$task_type)
}

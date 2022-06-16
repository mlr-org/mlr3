#' @title PredictionData
#'
#' @description
#' Convert object to a [PredictionData] or a list of [PredictionData].
#'
#' @inheritParams as_task
#' @param task ([Task]).
#' @template param_row_ids
#' @param check (`logical(1)`)\cr
#'   Perform argument checks and type conversions?
#'
#' @return [PredictionData].
#' @export
as_prediction_data = function(x, task, row_ids = task$row_ids, check = TRUE, ...) {
  UseMethod("as_prediction_data")
}

#' @rdname as_prediction_data
#' @export
as_prediction_data.Prediction = function(x, task, row_ids = task$row_ids, check = TRUE, ...) { # nolint
  x$data
}

#' @rdname as_prediction_data
#' @export
as_prediction_data.PredictionData = function(x, task, row_ids = task$row_ids, check = TRUE, ...) { # nolint
  x
}

#' @rdname as_prediction_data
#' @export
as_prediction_data.list = function(x, task, row_ids = task$row_ids, check = TRUE, ...) { # nolint
  assert_list(x, names = "unique")
  predict_types = names(mlr_reflections$learner_predict_types[[task$task_type]])
  assert_names(names(x), subset.of = predict_types)

  x$row_ids = row_ids
  if (inherits(task, "TaskSupervised")) {
    x$truth = task$truth(row_ids)
  }

  pdata = new_prediction_data(x, class = fget(mlr_reflections$task_generators, task$task_type, "prediction_data", "type"))
  if (check) {
    pdata = check_prediction_data(pdata)
  }

  pdata
}

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
#' @param ... (any)\cr
#'   Additional arguments.
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
#' @param train_task ([Task])\cr
#'   Task used for training the learner.
#' @export
as_prediction_data.list = function(x, task, row_ids = task$row_ids, check = TRUE, ..., train_task) { # nolint
  assert_list(x, names = "unique")
  predict_types = names(mlr_reflections$learner_predict_types[[task$task_type]])
  assert_names(names(x), subset.of = predict_types)

  x$row_ids = row_ids
  if (inherits(task, "TaskSupervised")) {
    x$truth = task$truth(row_ids)
  }

  task = if (task$task_type == "unsupervised") train_task else task
  pdata = new_prediction_data(x, task$task_type)
  if (check) {
    pdata = check_prediction_data(pdata, train_task = task)
  }

  pdata
}

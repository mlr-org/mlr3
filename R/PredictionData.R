#' @title PredictionData
#'
#' @name PredictionData
#' @rdname PredictionData
#'
#' @description
#' Objects of type `PredictionData` serve as a intermediate representation for objects of type [Prediction].
#' It is an internal data structure, implemented to optimize runtime and solve some issues emerging while serializing R6 objects.
#' End-users typically do not need to worry about the details, package developers are advised to continue reading for some technical information.
#'
#' Unlike most other \CRANpkg{mlr3} objects, `PredictionData` relies on the S3 class system.
#' The following operations must be supported to extend mlr3 for new task types:
#'
#' * `as_prediction_data()` converts objects to class `PredictionData`, e.g. objects of type [Prediction].
#' * `as_prediction()` converts objects to class [Prediction], e.g. objects of type `PredictionData`.
#' * `check_prediction_data()` is called on the return value of the predict method of a [Learner] to perform assertions and type conversions.
#'   Returns an update object of class `PredictionData`.
#' * `is_missing_prediction_data()` is used for the fallback learner (see [Learner]) to impute missing predictions. Returns vector with row ids which need imputation.
#'
#'
NULL

new_prediction_data = function(li, task_type = NULL) {
  capitalize = function(str) {
    substr(str, 1L, 1L) = toupper(substr(str, 1L, 1L))
    str
  }

  li = discard(li, is.null)
  class(li) = sprintf("PredictionData%s", c(capitalize(task_type), ""))
  li
}

#' @export
print.PredictionData = function(x, ...) {
  catf("<%s:%i>", class(x)[1L], length(x$row_ids))
}

#' @rdname PredictionData
#' @param pdata ([PredictionData])\cr
#'   Named list inheriting from `"PredictionData"`.
#' @export
check_prediction_data = function(pdata) {
  UseMethod("check_prediction_data")
}

#' @rdname PredictionData
#' @export
is_missing_prediction_data = function(pdata) {
  UseMethod("is_missing_prediction_data")
}


#' @rdname PredictionData
#' @param x (any)\cr
#'   Object to convert.
#' @param task ([Task]).
#' @param row_ids (`integer()`).
#' @param check (`logical(1)`)\cr
#'   Perform argument checks and conversions?
#' @export
as_prediction_data = function(x, task, row_ids = task$row_ids, check = TRUE) {
  UseMethod("as_prediction_data")
}

#' @export
#' @rdname PredictionData
as_prediction_data.Prediction = function(x, task, row_ids = task$row_ids, check = TRUE) { # nolint
  x$data
}

#' @rdname PredictionData
#' @export
as_prediction_data.PredictionData = function(x, task, row_ids = task$row_ids, check = TRUE) { # nolint
  x
}

#' @rdname PredictionData
#' @export
as_prediction_data.list = function(x, task, row_ids = task$row_ids, check = TRUE) { # nolint
  assert_list(x, names = "unique")
  predict_types = names(mlr_reflections$learner_predict_types[[task$task_type]])
  assert_names(names(x), subset.of = predict_types)

  x$row_ids = row_ids
  if (inherits(task, "TaskSupervised"))
    x$truth = task$truth(row_ids)

  pdata = new_prediction_data(x, task_type = task$task_type)
  if (check) {
    pdata = check_prediction_data(pdata)
  }

  pdata
}


#' @export
#' @rdname PredictionData
as_prediction = function(x, check = TRUE) {
  if (is.null(x)) {
    return(NULL)
  }
  UseMethod("as_prediction")
}

#' @export
#' @rdname PredictionData
as_prediction.Prediction = function(x, check = TRUE) { # nolint
  x
}

as_predictions = function(x, predict_sets = "test") { # nolint
  assert_subset(predict_sets, mlr_reflections$predict_sets)
  map(x, function(li) {
    assert_list(li, "PredictionData")
    as_prediction(do.call(c, discard(li[predict_sets], is.null)), check = FALSE)
  })
}

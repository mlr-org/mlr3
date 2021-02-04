#' @title Convert to PredictionData
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
#' * [as_prediction_data()] converts objects to class `PredictionData`, e.g. objects of type [Prediction].
#' * [as_prediction()] converts objects to class [Prediction], e.g. objects of type `PredictionData`.
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

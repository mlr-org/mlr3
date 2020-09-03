#' @export
check_prediction_data = function(pdata) {
  UseMethod("check_prediction_data")
}

#' @export
print.PredictionData = function(x, ...) {
  catf("<%s:%i>", class(x)[1L], length(x$row_ids))
}

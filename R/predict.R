#' @export
predict.Experiment = function(object, subset = NULL, newdata = NULL, ...) {
  if (...length() > 0L)
    stopf("predict: dotargs currently unsupported!")
  object$clone()$predict(subset, newdata)
}

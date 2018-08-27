#' @export
predict.Experiment = function(object, subset = NULL, newdata = NULL, ...) {
  if (...length() > 0L)
    stop("predict: dotargs currently unsupported!")
  experiment_predict(object$clone(), subset = subset, newdata = newdata)
}


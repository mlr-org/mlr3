#' @include Measure.R
mlr.measures$add(Measure$new(
  id = "mmce",
  description = "Mean misclassification error",
  task.types = c("classif"),
  fun = function(truth, predicted) {
    mean(truth != predicted)
  }
))

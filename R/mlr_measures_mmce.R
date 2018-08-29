#' @include Measure.R
mlr_measures$add(Measure$new(
  id = "mmce",
  description = "Mean misclassification error",
  task_types = c("classif"),
  fun = function(truth, predicted) {
    mean(truth != predicted)
  },
  aggregator = mean
))

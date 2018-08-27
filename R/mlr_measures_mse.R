#' @include Measure.R
mlr_measures$add(Measure$new(
  id = "mse",
  description = "Mean squared error",
  task_types = c("regr"),
  fun = function(truth, predicted) {
    mean( (truth - predicted)^2 )
  }
))

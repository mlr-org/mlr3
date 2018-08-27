#' @title Score a Model on a Task
#'
#' @description
#' Scores the fitted model on a (new) subset of the task.
#'
#' @param experiment [Experiment].\cr
#'   Experiment with fitted model and predictions.
#' @param measures [Measure]\cr
#'   Measure or list of measures.
#' @return \code{\link{Experiment}}.
#' @export
score = function(experiment, measures = NULL) {
  assert_experiment(experiment)
  experiment_score(experiment$clone(), measures)
}

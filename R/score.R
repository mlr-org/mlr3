#' @export
score = function(experiment, measures = NULL) {
  assert_experiment(experiment)
  experiment_score(experiment$clone(), measures)
}

#' @title Train a Learner on a Task
#'
#' @description
#' Fits a model on a subset of the task data.
#'
#' @param task ([Task()])\cr
#'   Object of type [Task()].
#' @param learner ([Learner()])\cr
#'   Object of type [Learner()].
#' @param subset (`integer` | `logical`)\cr
#'   Subset of `task` to train the data on.
#'   See [subsetting-types].
#' @export
#' @return [Experiment].
train = function(task, learner, subset = NULL) {
  e = Experiment$new(task = task, learner = learner)
  experiment_train(e, subset = subset)
}

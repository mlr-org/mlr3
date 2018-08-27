#' @title Train a Learner on a Task
#'
#' @description
#' Fits a model on (a subset of) the task data.
#'
#' @param task [\code{\link{Task}}]\cr
#'   Object of type \code{\link{Task}}.
#' @param learner [\code{\link{Learner}}]\cr
#'   Object of type \code{\link{Learner}}.
#' @param subset [\code{integer} | \code{logical}]\cr
#'   Subset of \code{task} to train the data on as row indices.
#'   See \code{\link{subsetting-types}}.
#' @return \code{\link{TrainResult}}.
#' @export
train = function(task, learner, subset = NULL) {
  e = Experiment$new(task = task, learner = learner)
  experiment_train(e, subset = subset)
}


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
  trainExperiment(e, subset = subset)
}

trainExperiment = function(e, subset) {
  train.set = e$data$task$row.ids(subset)
  e$data$resampling = ResamplingCustom$new()$instantiate(e$data$task, train.sets = list(train.set))
  e$data$iteration = 1L

  future = future::futureCall(
    trainWorker,
    c(e$data[c("task", "learner")], list(train.set = train.set)),
    globals = FALSE)
  value = future::value(future)
  e$data = insert(e$data, value)
  e$data = insert(e$data, list(test.time = NULL, test.log = NULL, predicted = NULL, performance = NULL))
  return(e)
}

trainWorker = function(task, learner, train.set) {
  pkgs = c("mlr3", learner$packages)
  requireNamespaces(pkgs, sprintf("The following packages are required for learner %s: %%s", learner$id))

  pars = c(list(task = task, row.ids = train.set), learner$par.vals)
  fun = learner$train
  now = proc.time()[[3L]]
  res = ecall(fun, pars)
  if (is.null(res$result)) {
    dummy = mlr.learners$get(sprintf("%s.dummy", learner$task.type))
    res$result = do.call(dummy$train, pars)
  }

  return(list(
    model = res$result,
    train.time = round(proc.time()[[3L]] - now, 8L),
    train.log = res$log
  ))
}

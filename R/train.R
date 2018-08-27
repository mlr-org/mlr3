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
  train_experiment(e, subset = subset)
}

train_experiment = function(e, subset) {
  train_set = e$data$task$row_ids(subset)
  e$data$resampling = ResamplingCustom$new()$instantiate(e$data$task, train_sets = list(train_set))
  e$data$iteration = 1L

  future = future::futureCall(
    train_worker,
    c(e$data[c("task", "learner")], list(train_set = train_set)),
    globals = FALSE)
  value = future::value(future)
  e$data = insert(e$data, value)
  e$data = insert(e$data, list(test_time = NULL, test_log = NULL, predicted = NULL, performance = NULL))
  return(e)
}

train_worker = function(task, learner, train_set) {
  pkgs = c("mlr3", learner$packages)
  require_namespaces(pkgs, sprintf("The following packages are required for learner %s: %%s", learner$id))

  pars = c(list(task = task, row_ids = train_set), learner$par_vals)
  fun = learner$train
  now = proc.time()[[3L]]
  res = ecall(fun, pars)
  if (is.null(res$result)) {
    dummy = mlr_learners$get(sprintf("%s.dummy", learner$task_type))
    res$result = do.call(dummy$train, pars)
  }

  return(list(
    model = res$result,
    train_time = round(proc.time()[[3L]] - now, 8L),
    train_log = res$log
  ))
}

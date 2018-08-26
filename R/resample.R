#' @title Resample a Learner on a Task
#'
#' @description
#' Runs a resampling (possibly in parallel).
#'
#' @param task [\code{\link{Task}}]\cr
#'   Object of type \code{\link{Task}}.
#' @param learner [\code{\link{Learner}}]\cr
#'   Object of type \code{\link{Learner}}.
#' @param resampling [\code{\link{Resampling}}]\cr
#'   Object of type \code{\link{Resampling}}.
#' @param measures [\code{list} of \code{\link{Measure}}]\cr
#'   List of objects of type \code{\link{Measure}}.
#' @return \code{\link{ResampleResult}}.
#' @export
resample = function(task, learner, resampling, measures) {
  assertTask(task)
  assertLearner(learner, task = task)
  assertResampling(resampling)
  measures = asMeasures(measures, task = task)

  if (resampling$is_instantiated) {
    instance = resampling$clone() # FIXME: clone
  } else {
    instance = resampling$instantiate(task) # FIXME: clone
  }
  n = instance$iters

  res = future.apply::future_lapply(seq_len(n), function(i, task, learner, instance, measures) {
    train.set = instance$train.set(i)
    test.set = instance$test.set(i)
    mlr3:::runExperiment(task = task, learner = learner,  train.set = train.set, test.set = test.set, measures = measures)
  }, future.globals = FALSE, future.packages = "mlr3", task = task, learner = learner, instance = instance, measures = measures)


  res = combineExperiments(res)
  res[, c("task", "learner", "resampling", "iteration") := list(list(task), list(learner), list(instance), seq_len(n))]
  ResampleResult$new(res)
}

ResampleResult = R6Class("ResampleResult",
  cloneable = FALSE,
  public = list(
    data = NULL,

    initialize = function(data) {
      assertDataTable(data)
      slots = capabilities$experiment.slots$name
      assertNames(names(data), permutation.of = slots)
      self$data = setcolorder(data, slots)[]
    },

    print = function(...) {
      catf("ResamplingResult of learner '%s' on task '%s' with %i iterations", self$task$id, self$learner$id, nrow(self$data))
      # vapply(self$performance[, !"iteration"], function(x) c(mean(x), sd(x)))
      # ... TBC
    },

    experiment = function(iteration) {
      iteration = asInt(iteration, lower = 1L, upper = nrow(self$data))
      .mapply(Experiment$new, self$data[iteration], MoreArgs = list())[[1L]]
    },

    experiments = function(iterations) {
      iterations = asInteger(iterations, lower = 1L, upper = nrow(self$data), any.missing = FALSE)
      .mapply(Experiment$new, self$data[iterations], MoreArgs = list())
    }

  ),

  active = list(
    performance = function() {
      perf = rbindlist(lapply(self$data$performance, as.list), fill = TRUE)
      cbind(data.table(iteration = seq_row(self$data)), perf)
    },

    task = function() {
      self$data$task[[1L]]
    },

    learner = function() {
      self$data$learner[[1L]]
    },

    resampling = function() {
      self$data$resampling[[1L]]
    },

    measures = function() {
      names(self$data$performance[[1L]])
    }
  )
)

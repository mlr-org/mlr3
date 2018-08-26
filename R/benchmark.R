#' @title Benchmark Multiple Learners on Multiple Tasks
#'
#' @description
#' Runs a benchmark on the full grid of learners and tasks.
#'
#' @param tasks [\code{list} of \code{\link{Task}}]\cr
#'   List of objects of type \code{\link{Task}}.
#' @param learners [\code{list} of \code{\link{Learner}}]\cr
#'   List of objects of type \code{\link{Learner}}.
#' @param resamplings [\code{list} of \code{\link{Resampling}}]\cr
#'   List of objects of type \code{\link{Resampling}}.
#' @param measures [\code{list} of \code{\link{Measure}}]\cr
#'   List of objects of type \code{\link{Measure}}.
#' @return \code{\link{BenchmarkResult}}.
#' @export
#' @examples
#' tasks = lapply(c("iris", "sonar"), mlr.tasks$get)
#' learners = lapply(c("classif.dummy", "classif.rpart"), mlr.learners$get)
#' resamplings = lapply("cv", mlr.resamplings$get)
#' measures = lapply("mmce", mlr.measures$get)
#' benchmark(tasks, learners, resamplings, measures)
benchmark = function(tasks, learners, resamplings, measures) {
  assertList(tasks, "Task", min.len = 1L)
  assertList(learners, "Learner", min.len = 1L)
  assertList(resamplings, "Resampling", min.len = 1L)
  assertList(measures, "Measure", min.len = 1L)
  names(tasks) = assertNames(ids(tasks), "unique")
  names(learners) = assertNames(ids(learners), "unique")
  names(resamplings) = assertNames(ids(resamplings), "unique")
  names(measures) = assertNames(ids(measures), "unique")


  # Instantiate resampling for each task
  grid = CJ(task = names(tasks), resampling = names(resamplings))
  instances = .mapply(function(task, resampling) resamplings[[resampling]]$clone()$instantiate(tasks[[task]]), grid, list())
  names(instances) = grid$instance = vcapply(instances, "[[", "checksum")

  # Cross join learner x task combinations
  tmp = CJ(task = names(tasks), learner = names(learners))
  grid = grid[tmp, on = "task", allow.cartesian = TRUE]

  # Cross join resampling iterations
  tmp = rbindlist(lapply(instances, function(x) { data.table(instance = x$checksum, iter = seq_len(x$iters)) }))
  grid = grid[tmp, on = "instance", allow.cartesian = TRUE]

  # prepare result data.table
  res = data.table(
    task = tasks[grid$task],
    learner = learners[grid$learner],
    resampling = instances[grid$instance],
    iteration = grid$iter
  )

  tmp = future.apply::future_mapply(runExperiment,
    task = res$task,
    learner = res$learner,
    train.set = .mapply(function(instance, iter, ...) instances[[instance]]$train.set(iter), grid, list()),
    test.set = .mapply(function(instance, iter, ...) instances[[instance]]$test.set(iter), grid, list()),
    MoreArgs = list(measures = measures),
    SIMPLIFY = FALSE,
    USE.NAMES = FALSE,
    future.globals = FALSE, future.packages = "mlr3")
  tmp = combineExperiments(tmp)
  res[, names(tmp) := tmp]

  BenchmarkResult$new(res)
}

BenchmarkResult = R6Class("BenchmarkResult",
  cloneable = FALSE,
  public = list(
    data = NULL,

    initialize = function(data) {
      assertDataTable(data)
      slots = capabilities$experiment.slots$name
      assertNames(names(data), permutation.of = slots)
      self$data = setcolorder(data, slots)
    },

    experiment = function(i) {
      assertInt(i, lower = 1L, upper = nrow(self$data))
      .mapply(Experiment$new, self$data[i], MoreArgs = list())[[1L]]
    },

    experiments = function(i) {
      assertInteger(i, lower = 1L, upper = nrow(self$data), any.missing = FALSE)
      .mapply(Experiment$new, self$data[i], MoreArgs = list())
    }

  ),

  active = list(
    performance = function() {
      tmp = self$data[, list(task = ids(task), learner = ids(learner), performance = performance)]
      cbind(tmp[, !"performance"], rbindlist(tmp$performance, fill = TRUE))
    }
  )
)

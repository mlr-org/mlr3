#' @title Benchmark Multiple Learners on Multiple Tasks
#'
#' @description
#' Runs a benchmark of the cross-product of learners, tasks, and resampling strategies (possibly in parallel).
#'
#'
#' @param tasks (`list` of [Task])\cr
#'   List of objects of type [Task].
#' @param learners (`list` of [Learner])\cr
#'   List of objects of type [Learner].
#' @param resamplings (`list` of [Resampling])\cr
#'   List of objects of type [Resampling].
#' @return [BenchmarkResult].
#' @export
#' @examples
#' tasks = mlr_tasks$mget(c("iris", "sonar"))
#' learners = lapply(c("classif.dummy", "classif.rpart"), mlr_learners$get)
#' resamplings = lapply("cv", mlr_resamplings$get)
#' bmr = benchmark(tasks, learners, resamplings)
#' bmr$performance
benchmark = function(tasks, learners, resamplings) {
  assert_list(tasks, "Task", min.len = 1L)
  assert_list(learners, "Learner", min.len = 1L)
  assert_list(resamplings, "Resampling", min.len = 1L)
  names(tasks) = assert_names(ids(tasks), "unique")
  names(learners) = assert_names(ids(learners), "unique")
  names(resamplings) = assert_names(ids(resamplings), "unique")


  # Instantiate resampling for each task
  grid = CJ(task = names(tasks), resampling = names(resamplings))
  instances = .mapply(function(task, resampling) resamplings[[resampling]]$clone()$instantiate(tasks[[task]]), grid, list())
  names(instances) = grid$instance = vcapply(instances, "[[", "checksum")

  # Cross join task x learner combinations
  tmp = CJ(task = names(tasks), learner = names(learners))
  grid = grid[tmp, on = "task", allow.cartesian = TRUE]

  # Cross join resampling iterations
  tmp = rbindlist(lapply(instances, function(x) { data.table(instance = x$checksum, iter = seq_len(x$iters)) }))
  grid = grid[tmp, on = "instance", allow.cartesian = TRUE]

  # prepare result data.table
  #
  # we randomize the order here to increase ACET
  # this should be controllable by the user though:
  # https://github.com/HenrikBengtsson/future.apply/issues/25
  ii = sample.int(nrow(grid))
  res = data.table(
    task = tasks[grid$task],
    learner = learners[grid$learner],
    resampling = instances[grid$instance],
    iteration = grid$iter
  )[ii]

  tmp = future.apply::future_mapply(experiment_worker,
    task = res$task,
    learner = res$learner,
    resampling = res$resampling,
    iteration = res$iteration,
    MoreArgs = list(ctrl = mlr_options()),
    SIMPLIFY = FALSE,
    USE.NAMES = FALSE,
    future.globals = FALSE,
    future.packages = "mlr3"
  )

  tmp = combine_experiments(tmp)
  res[, names(tmp) := tmp]
  res = res[order(ids(task), ids(learner), iteration)]

  BenchmarkResult$new(res)
}

#' @title Container for Results of benchmark
#'
#' @export
BenchmarkResult = R6Class("BenchmarkResult",
  cloneable = FALSE,
  public = list(
    data = NULL,

    initialize = function(data) {
      assert_data_table(data)
      slots = reflections$experiment_slots$name
      assert_names(names(data), permutation.of = slots)
      self$data = setcolorder(data, slots)
    },

    experiment = function(i) {
      assert_int(i, lower = 1L, upper = nrow(self$data))
      .mapply(Experiment$new, self$data[i], MoreArgs = list())[[1L]]
    },

    experiments = function(i) {
      assert_integer(i, lower = 1L, upper = nrow(self$data), any.missing = FALSE)
      .mapply(Experiment$new, self$data[i], MoreArgs = list())
    },

    performances = function() {
      tmp = self$data[, list(task = ids(task), learner = ids(learner), performance = performance)]
      cbind(tmp[, !"performance"], rbindlist(tmp$performance, fill = TRUE))
    }

  ),

  active = list(
    performance = function() {
      tmp = self$data[, list(task = ids(task), learner = ids(learner), performance = performance)]
      cbind(tmp[, !"performance"], rbindlist(tmp$performance, fill = TRUE))
    }
  )
)

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
#' learners = mlr_learners$mget(c("classif.dummy", "classif.rpart"))
#' resamplings = mlr_resamplings$mget("cv")
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

  if (use_future()) {
    debug("Running benchmark() sequentially with %i iterations", nrow(res))
    tmp = mapply(experiment_worker,
      task = res$task, learner = res$learner, resampling = res$resampling, iteration = res$iteration,
      MoreArgs = list(ctrl = mlr_options()), SIMPLIFY = FALSE, USE.NAMES = FALSE
    )

  } else {
    debug("Running resample() via future with %i iterations", nrow(res))
    tmp = future.apply::future_mapply(experiment_worker,
      task = res$task, learner = res$learner, resampling = res$resampling, iteration = res$iteration,
      MoreArgs = list(ctrl = mlr_options()), SIMPLIFY = FALSE, USE.NAMES = FALSE,
      future.globals = FALSE, future.packages = "mlr3"
    )
  }

  tmp = combine_experiments(tmp)
  res[, names(tmp) := tmp]
  res = res[order(ids(get("task")), ids(get("learner")), get("iteration"))]

  BenchmarkResult$new(res)
}

#' @title Generate a Benchmark Grid Design
#'
#' @description
#' Takes a lists of [Task], a list of [Learner] and a list of [Resampling] to
#' generate a design in an [expand.grid()] fashion (a.k.a. cross join or Cartesian product).
#'
#' There are two modes of operation, depending on the flag `paired`.
#'
#' * With `paired` set to `FALSE` (default), resampling strategies are not allowed to be instantiated, and instead will be instantiated per task internally.
#'   The only exception to this rule applies if all tasks have exactly the same number of rows, and the resamplings are all instantiated for such tasks.
#'   The grid will be generated based on the Cartesian product of tasks, learners, and resamplings.
#'   Because the resamplings are instantiated on the tasks, reproducibility requires a seed to be set **before**
#'   calling this function, as this process is stochastic.
#' * With `paired` set to `TRUE`, tasks and resamplings are treated as pairs.
#'   I.e., you must provide as many tasks as corresponding instantiated resamplings.
#'   The grid will be generated based on the Cartesian product of learners and pairs.
#'
#' @param tasks (list of [Task]).
#' @param learners (list of [Learner]).
#' @param resamplings (list of [Resampling]).
#' @param paired (`logical(1)`)\cr
#'   Set this to `TRUE` if the resamplings are instantiated on the tasks, i.e., the tasks and resamplings are paired.
#'   You need to provide the same number of tasks and instantiated resamplings.
#'
#' @return ([data.table::data.table()]) with the cross product of the input vectors.
#'
#' @template seealso_benchmark
#' @export
#' @examples
#' tasks = list(tsk("penguins"), tsk("sonar"))
#' learners = list(lrn("classif.featureless"), lrn("classif.rpart"))
#' resamplings = list(rsmp("cv"), rsmp("subsampling"))
#'
#' # Set a seed to ensure reproducibility of the resampling instantiation
#' set.seed(123)
#' grid = benchmark_grid(tasks, learners, resamplings)
#' # the resamplings are now instantiated
#' head(grid$resampling[[1]]$instance)
#' print(grid)
#' \dontrun{
#' benchmark(grid)
#' }
#'
#' # paired
#' learner = lrn("classif.rpart")
#' task1 = tsk("penguins")
#' task2 = tsk("german_credit")
#' res1 = rsmp("holdout")
#' res2 = rsmp("holdout")
#' res1$instantiate(task1)
#' res2$instantiate(task2)
#' design = benchmark_grid(list(task1, task2), learner, list(res1, res2), paired = TRUE)
#' print(design)
#'
#' # manual construction of the grid with data.table::CJ()
#' grid = data.table::CJ(task = tasks, learner = learners,
#'   resampling = resamplings, sorted = FALSE)
#'
#' # manual instantiation (not suited for a fair comparison of learners!)
#' Map(function(task, resampling) {
#'   resampling$instantiate(task)
#' }, task = grid$task, resampling = grid$resampling)
#' \dontrun{
#' benchmark(grid)
#' }
#'
benchmark_grid = function(tasks, learners, resamplings, paired = FALSE) {
  tasks = assert_tasks(as_tasks(tasks))
  learners = assert_learners(as_learners(learners))
  resamplings = assert_resamplings(as_resamplings(resamplings))

  if (assert_flag(paired)) {
    if (length(tasks) != length(resamplings)) {
      stopf("If `paired` is `TRUE`, you need to provide the same number of tasks and instantiated resamplings")
    }

    for (i in seq_along(tasks)) {
      task = tasks[[i]]
      resampling = resamplings[[i]]
      if (!resamplings[[i]]$is_instantiated) {
        stopf("Resampling #%i ('%s' for task '%s') is not instantiated", i, resampling$id, task$id)
      }
      if (resampling$task_hash != task$hash) {
        stopf("Resampling #%i ('%s' for task '%s') is not instantiated on the corresponding task", i, resampling$id, task$id)
      }
    }

    grid = CJ(task = seq_along(tasks), learner = seq_along(learners))
    grid$instance = seq_row(grid)

    tab = data.table(
      task = tasks[grid$task],
      learner = learners[grid$learner],
      resampling = resamplings[grid$task]
    )
  } else {
    grid = CJ(task = seq_along(tasks), resampling = seq_along(resamplings))
    is_instantiated = map_lgl(resamplings, "is_instantiated")

    if (any(is_instantiated)) {
      task_nrow = unique(map_int(tasks, "nrow"))
      if (length(task_nrow) != 1L) {
        stopf("All resamplings must be uninstantiated, or must operate on tasks with the same number of rows")
      }
      if (!identical(task_nrow, unique(map_int(resamplings, "task_nrow")))) {
        stop("A Resampling is instantiated for a task with a different number of observations")
      }
      instances = pmap(grid, function(task, resampling) resamplings[[resampling]]$clone())
    } else {
      instances = pmap(grid, function(task, resampling) resamplings[[resampling]]$clone()$instantiate(tasks[[task]]))
    }

    grid$instance = seq_row(grid)
    grid = grid[CJ(task = seq_along(tasks), learner = seq_along(learners)), on = "task", allow.cartesian = TRUE]

    tab = data.table(task = tasks[grid$task], learner = learners[grid$learner], resampling = instances[grid$instance])
  }

  set_data_table_class(tab, "benchmark_grid")
  return(tab)
}

#' @export
print.benchmark_grid = function(x, ...) {
  task_ids = map(x$task, "id")
  learner_ids = map(x$learner, "id")
  resampling_ids = map(x$resampling, "id")
  print(data.table(task = task_ids, learner = learner_ids, resampling = resampling_ids))
}

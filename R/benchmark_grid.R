#' @title Generate a Benchmark Grid Design
#'
#' @description
#' Takes a lists of [Task], a list of [Learner] and a list of [Resampling] to
#' generate a design in an [expand.grid()] fashion (a.k.a. cross join or Cartesian product).
#'
#' Resampling strategies are not allowed to be instantiated when passing the argument, and instead will be instantiated per task internally.
#' The only exception to this rule applies if all tasks have exactly the same number of rows, and the resamplings are all instantiated for such tasks.
#'
#' @param tasks (list of [Task]).
#' @param learners (list of [Learner]).
#' @param resamplings (list of [Resampling]).
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
#' grid = benchmark_grid(tasks, learners, resamplings)
#' print(grid)
#' \dontrun{
#' benchmark(grid)
#' }
#'
#' # manual construction of the grid with data.table::CJ()
#' grid = CJ(task = tasks, learner = learners, resampling = resamplings,
#'   sorted = FALSE)
#'
#' # manual instantiation (not suited for a fair comparison of learners!)
#' Map(function(task, resampling) {
#'    resampling$instantiate(task)
#' }, task = grid$task, resampling = grid$resampling)
#'
#' \dontrun{
#' benchmark(grid)
#' }
benchmark_grid = function(tasks, learners, resamplings) {
  tasks = assert_tasks(as_tasks(tasks))
  learners = assert_learners(as_learners(learners))
  resamplings = assert_resamplings(as_resamplings(resamplings))

  grid = CJ(task = seq_along(tasks), resampling = seq_along(resamplings))
  is_instantiated = map_lgl(resamplings, "is_instantiated")

  if (any(is_instantiated)) {
    task_nrow = unique(map_int(tasks, "nrow"))
    if (length(task_nrow) != 1L) {
      stopf("All tasks must be uninstantiated, or must have the same number of rows")
    }
    if (!identical(task_nrow, unique(map_int(resamplings, "task_nrow")))) {
      stop("Resampling is instantiated for a task with a different number of observations")
    }
    instances = pmap(grid, function(task, resampling) resamplings[[resampling]]$clone())
  } else {
    instances = pmap(grid, function(task, resampling) resamplings[[resampling]]$clone()$instantiate(tasks[[task]]))
  }

  grid$instance = seq_row(grid)
  grid = grid[CJ(task = seq_along(tasks), learner = seq_along(learners)), on = "task", allow.cartesian = TRUE]

  data.table(task = tasks[grid$task], learner = learners[grid$learner], resampling = instances[grid$instance])
}

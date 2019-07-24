#' @title Generate a Benchmark Design
#'
#' @description
#' Takes a lists of [Task], a list of [Learner] and a list of [Resampling] to
#' generate a design in an [expand.grid()] fashion (a.k.a. cross join or Cartesian product).
#'
#' Resampling strategies may not be instantiated, and will be instantiated per task internally.
#'
#' @param tasks :: list of [Task].
#' @param learners :: list of [Learner].
#' @param resamplings :: list of [Resampling].
#'
#' @template section-sugar
#' @return ([data.table::data.table()]) with the cross product of the input vectors.
#' @export
expand_grid = function(tasks, learners, resamplings) {

  tasks = assert_tasks(tasks)
  learners = assert_learners(learners)
  resamplings = assert_resamplings(resamplings)
  assert_resamplings(resamplings, instantiated = FALSE)

  grid = CJ(task = seq_along(tasks), resampling = seq_along(resamplings))
  instances = pmap(grid, function(task, resampling) resamplings[[resampling]]$clone()$instantiate(tasks[[task]]))
  grid$instance = seq_row(grid)
  grid = grid[CJ(task = seq_along(tasks), learner = seq_along(learners)), on = "task", allow.cartesian = TRUE]

  data.table(task = tasks[grid$task], learner = learners[grid$learner], resampling = instances[grid$instance])
}

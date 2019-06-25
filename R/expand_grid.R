#' @title Generate a Benchmark Design
#'
#' @description
#' Takes a lists of [Task], a list of [Learner] and a list of [Resampling] to
#' generate a design in an [expand.grid()] fashion (a.k.a. cross join or Cartesian product).
#'
#' Resampling strategies may not be instantiated, and will be instantiated per task internally.
#'
#' @param tasks :: (list of [Task] | `character()`)\cr
#'   Instead a [Task] object, it is also possible to provide a keys to retrieve tasks from the [mlr_tasks] dictionary.
#' @param learners (list of [Learner] | `character()`)\cr
#'   Instead if a [Learner] object, it is also possible to provide keys to retrieve learners from the [mlr_learners] dictionary.
#' @param resamplings :: (list of [Resampling] | `character()`)\cr
#'   Instead if a [Resampling] object, it is also possible to provide a key to retrieve a resampling from the [mlr_resamplings] dictionary.
#'
#' @return ([data.table()]) with the cross product of the input vectors.
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

  design = data.table(task = tasks[grid$task], learner = learners[grid$learner], resampling = instances[grid$instance])
  attr(design, "exhaustive_grid") = TRUE
  design
}

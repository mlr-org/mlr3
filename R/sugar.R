retrieve_from_dict = function(x, type, dict, clone = FALSE) {
  if (inherits(x, type)) {
    if (clone)
      return(x$clone(deep = TRUE))
    return(x)
  }

  if (is.character(x) && dict$has(x)) {
    return(dict$get(x))
  }

  stopf("Invalid %s specification", type)
}

get_task = function(task, clone = FALSE)  {
  retrieve_from_dict(task, "Task", mlr_tasks, clone = clone)
}

get_tasks = function(tasks, clone = FALSE) {
  if (inherits(tasks, "Task"))
    return(list(tasks))
  tasks = map(tasks, get_task, clone = clone)
  types = unique(map_chr(tasks, "task_type"))
  if (length(types) > 1L)
    stopf("All tasks must have the same task type (types found: %s)", str_collapse(types))
  tasks
}

get_learner = function(learner, clone = FALSE) {
  retrieve_from_dict(learner, "Learner", mlr_learners, clone = clone)
}

get_learners = function(learners, clone = FALSE) {
  if (inherits(learners, "Learner"))
    return(list(learners))
  learners = map(learners, get_learner, clone = clone)
  types = unique(map_chr(learners, "task_type"))
  if (length(types) > 1L)
    stopf("All learners must have the same task type (types found: %s)", str_collapse(types))
  learners
}

get_measure = function(measure, clone = FALSE) {
  retrieve_from_dict(measure, "Measure", mlr_measures, clone = clone)
}

get_measures = function(measures, clone = FALSE) {
  if (inherits(measures, "Measure"))
    return(list(measures))
  measures = map(measures, get_measure, clone = clone)
  types = unique(map_chr(measures, "task_type"))
  if (length(types) > 1L)
    stopf("All measures must have the same task type (types found: %s)", str_collapse(types))
  measures
}

get_resampling = function(resampling, clone = FALSE) {
  retrieve_from_dict(resampling, "Resampling", mlr_resamplings, clone = clone)
}

get_resamplings = function(resamplings, clone = FALSE) {
  if (inherits(resamplings, "Resampling"))
    return(list(resamplings))
  map(resamplings, get_resampling, clone = clone)
}

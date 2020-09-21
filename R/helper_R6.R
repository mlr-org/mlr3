#' @title Get 'private' part of an R6 Instance
#'
#' @param x ([R6::R6Class]).
#'
#' @return (`environment()`).
#' @noRd
get_private = function(x) {
  x[[".__enclos_env__"]][["private"]]
}


#' @title Sets the Feature Names in a Task
#'
#' @param learner ([Learner]).
#' @param feature_names (`list()`).
#'
#' @return list of ([Task]) with updated state.
#' @noRd
reassemble_tasks = function(tasks, feature_names = NULL) {
  tasks = lapply(tasks, function(t) t$clone())

  if (!is.null(feature_names))
     Map(function(t, fn) { t$col_roles$feature = fn }, t = tasks, fn = feature_names)

  tasks
}

#' @title Sets the State and/or ParamSet values in a Learner
#'
#' @param learner ([Learner]).
#' @param states (`list()`).
#' @param param_vals (`list()`).
#'
#' @return list of ([Learner]) with updated state and param values.
#' @noRd
reassemble_learners = function(learners, states = NULL, param_vals = NULL) {
  learners = lapply(learners, function(l) l$clone())

  if (!is.null(states))
     Map(function(l, s) { l$state = s }, l = learners, s = states)
  if (!is.null(param_vals))
    Map(function(l, pv) { p = get_private(l$param_set); p$.values = pv }, l = learners, pv = param_vals)
  learners
}

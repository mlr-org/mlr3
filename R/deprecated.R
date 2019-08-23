#' @export
#' @rdname mlr_coercions
as_task.character = function(x, clone = FALSE) {
  assert_string(x)
  mlr_tasks$get(x)
}

#' @export
#' @rdname mlr_coercions
as_tasks.character = function(x, clone = FALSE) {
  mlr_tasks$mget(x)
}

#' @rdname mlr_coercions
#' @export
as_learner.character = function(x, clone = FALSE)  {
  assert_string(x)
  mlr_learners$get(x)
}

#' @export
#' @rdname mlr_coercions
as_learners.character = function(x, clone = FALSE)  {
  mlr_learners$mget(x)
}

#' @export
#' @rdname mlr_coercions
as_resampling.character = function(x, clone = FALSE) {
  assert_string(x)
  mlr_resamplings$get(x)
}

#' @export
#' @rdname mlr_coercions
as_resamplings.character = function(x, clone = FALSE) {
  mlr_resamplings$mget(x)
}

#' @export
#' @rdname mlr_coercions
as_measure.character = function(x, task_type = NULL, clone = FALSE) {
  assert_string(x)
  mlr_measures$get(x)
}

#' @export
#' @rdname mlr_coercions
as_measures.character = function(x, task_type = NULL, clone = FALSE) {
  mlr_measures$mget(x)
}

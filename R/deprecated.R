#' @export
#' @rdname mlr_coercions
as_task.character = function(x, clone = FALSE) {
  .Deprecated("tsk", package = "mlr3", old = "Automatic object creation from strings in `mlr3`")
  assert_string(x)
  mlr_tasks$get(x)
}

#' @export
#' @rdname mlr_coercions
as_tasks.character = function(x, clone = FALSE) {
  .Deprecated("tsk", package = "mlr3", old = "Automatic object creation from strings in `mlr3`")
  mlr_tasks$mget(x)
}

#' @rdname mlr_coercions
#' @export
as_learner.character = function(x, clone = FALSE)  {
  .Deprecated("lrn", package = "mlr3", old = "Automatic object creation from strings in `mlr3`")
  assert_string(x)
  mlr_learners$get(x)
}

#' @export
#' @rdname mlr_coercions
as_learners.character = function(x, clone = FALSE)  {
  .Deprecated("lrn", package = "mlr3", old = "Automatic object creation from strings in `mlr3`")
  mlr_learners$mget(x)
}

#' @export
#' @rdname mlr_coercions
as_resampling.character = function(x, clone = FALSE) {
  .Deprecated("rsmp", package = "mlr3", old = "Automatic object creation from strings in `mlr3`")
  assert_string(x)
  mlr_resamplings$get(x)
}

#' @export
#' @rdname mlr_coercions
as_resamplings.character = function(x, clone = FALSE) {
  .Deprecated("rsmp", package = "mlr3", old = "Automatic object creation from strings in `mlr3`")
  mlr_resamplings$mget(x)
}

#' @export
#' @rdname mlr_coercions
as_measure.character = function(x, task_type = NULL, clone = FALSE) {
  .Deprecated("msr", package = "mlr3", old = "Automatic object creation from strings in `mlr3`")
  assert_string(x)
  mlr_measures$get(x)
}

#' @export
#' @rdname mlr_coercions
as_measures.character = function(x, task_type = NULL, clone = FALSE) {
  .Deprecated("msr", package = "mlr3", old = "Automatic object creation from strings in `mlr3`")
  mlr_measures$mget(x)
}

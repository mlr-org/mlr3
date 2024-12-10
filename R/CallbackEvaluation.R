#' @title Create Evaluation Callback
#'
#' @description
#' Callbacks allow to customize the behavior of `resample()` and `benchmark()` in mlr3.
#'
#' @export
CallbackEvaluation= R6Class("CallbackEvaluation",
  inherit = Callback,
  public = list(

    #' @field on_evaluation_begin (`function()`)\cr
    #' Stage called at the beginning of an evaluation.
    #' Called in `workhorse()` (internal).
    on_evaluation_begin = NULL,

    #' @field on_evaluation_before_train (`function()`)\cr
    #' Stage called before training the learner.
    #' Called in `workhorse()` (internal).
    on_evaluation_before_train = NULL,

    #' @field on_evaluation_before_predict (`function()`)\cr
    #' Stage called before predicting.
    #' Called in `workhorse()` (internal).
    on_evaluation_before_predict = NULL,

    #' @field on_evaluation_end (`function()`)\cr
    #' Stage called at the end of an evaluation.
    #' Called in `workhorse()` (internal).
    on_evaluation_end = NULL
  )
)

#' @title Create Workhorse Callback
#'
#' @description
#' Function to create a [CallbackEvaluation].
#' Predefined callbacks are stored in the [dictionary][mlr3misc::Dictionary] [mlr_callbacks] and can be retrieved with [clbk()].
#'
#' Evaluation callbacks are called at different stages of the resampling process.
#' The stages are prefixed with `on_*`.
#'
#' ```
#' Start Resampling Iteration on Worker
#'  - on_evaluation_begin
#'  - on_evaluation_before_train
#'  - on_evaluation_before_predict
#'  - on_evaluation_end
#' End Resampling Iteration on Worker
#' ```
#'
#' See also the section on parameters for more information on the stages.
#' A evaluation callback works with [ContextEvaluation].
#
#' @details
#' When implementing a callback, each function must have two arguments named `callback` and `context`.
#' A callback can write data to the state (`$state`), e.g. settings that affect the callback itself.
#' Evaluation callbacks access [ContextEvaluation].
#'
#' @param id (`character(1)`)\cr
#'  Identifier for the new instance.
#' @param label (`character(1)`)\cr
#'  Label for the new instance.
#' @param man (`character(1)`)\cr
#'  String in the format `[pkg]::[topic]` pointing to a manual page for this object.
#'  The referenced help package can be opened via method `$help()`.
#'
#' @param on_evaluation_begin (`function()`)\cr
#' Stage called at the beginning of an evaluation.
#' Called in `workhorse()` (internal).
#' @param on_evaluation_before_train (`function()`)\cr
#' Stage called before training the learner.
#' Called in `workhorse()` (internal).
#' @param on_evaluation_before_predict (`function()`)\cr
#' Stage called before predicting.
#' Called in `workhorse()` (internal).
#' @param on_evaluation_end (`function()`)\cr
#' Stage called at the end of an evaluation.
#' Called in `workhorse()` (internal).
#'
#' @export
callback_evaluation = function(
  id,
  label = NA_character_,
  man = NA_character_,
  on_evaluation_begin = NULL,
  on_evaluation_before_train = NULL,
  on_evaluation_before_predict = NULL,
  on_evaluation_end = NULL
  ) {
  stages = discard(set_names(list(
    on_evaluation_begin,
    on_evaluation_before_train,
    on_evaluation_before_predict,
    on_evaluation_end),
    c(
      "on_evaluation_begin",
      "on_evaluation_before_train",
      "on_evaluation_before_predict",
      "on_evaluation_end"
    )), is.null)

  stages = map(stages, function(stage) crate(assert_function(stage, args = c("callback", "context"))))
  callback = CallbackEvaluation$new(id, label, man)
  iwalk(stages, function(stage, name) callback[[name]] = stage)
  callback
}

#' @title Assertions for Callbacks
#'
#' @description
#' Assertions for [CallbackEvaluation] class.
#'
#' @param callback ([CallbackEvaluation]).
#' @param null_ok (`logical(1)`)\cr
#'   If `TRUE`, `NULL` is allowed.
#'
#' @return [CallbackEvaluation | List of [CallbackEvaluation]s.
#' @export
assert_evaluation_callback = function(callback, null_ok = FALSE) {
  if (null_ok && is.null(callback)) return(invisible(NULL))
  assert_class(callback, "CallbackEvaluation")
  invisible(callback)
}

#' @export
#' @param callbacks (list of [CallbackEvaluation]).
#' @rdname assert_evaluation_callback
assert_evaluation_callbacks = function(callbacks, null_ok = FALSE) {
  if (null_ok && is.null(callbacks)) return(invisible(NULL))
  invisible(lapply(callbacks, assert_evaluation_callback))
}

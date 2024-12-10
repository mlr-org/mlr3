#' @title Evaluation Callback
#'
#' @description
#' Specialized [mlr3misc::Callback] to customize the behavior of [resample()] and [benchmark()] in mlr3.
#' The [callback_evaluation()] function is used to create instances of this class.
#' Predefined callbacks are stored in the [dictionary][mlr3misc::Dictionary] [mlr_callbacks] and can be retrieved with [clbk()].
#' For more information on callbacks, see the [callback_evaluation()] documentation.
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

#' @title Create Evaluation Callback
#'
#' @description
#' Function to create a [CallbackEvaluation].
#' Predefined callbacks are stored in the [dictionary][mlr3misc::Dictionary] [mlr_callbacks] and can be retrieved with [clbk()].
#'
#' Evaluation callbacks are called at different stages of the resampling process.
#' Each stage is called once per resampling iteration.
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
#' An evaluation callback works with [ContextEvaluation].
#
#' @details
#' When implementing a callback, each function must have two arguments named `callback` and `context`.
#' A callback can write data to the state (`$state`), e.g. settings that affect the callback itself.
#' Evaluation callbacks access [ContextEvaluation].
#' Data can be stored in the [ResampleResult] and [BenchmarkResult] objects via `context$data_extra`.
#' Alternatively results can be stored in the learner state via `context$learner$state`.
#'
#' @param id (`character(1)`)\cr
#' Identifier for the new instance.
#' @param label (`character(1)`)\cr
#' Label for the new instance.
#' @param man (`character(1)`)\cr
#' String in the format `[pkg]::[topic]` pointing to a manual page for this object.
#' The referenced help package can be opened via method `$help()`.
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
#' @examples
#' callback = callback_evaluation("selected_features",
#'  label = "Selected Features",
#'
#'  on_evaluation_end = function(callback, context) {
#'     pred = as_prediction(context$pdatas$test)
#'     selected_features = pred$score(
#'       measure = msr("selected_features"),
#'       learner = context$learner,
#'       task = context$task)
#'     context$learner$state$selected_features = selected_features
#'   }
#' )
#'
#' task = tsk("pima")
#' learner = lrn("classif.rpart")
#' resampling = rsmp("cv", folds = 3)
#'
#' rr = resample(task, learner, resampling, callbacks = callback)
#'
#' rr$learners[[1]]$state$selected_features
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

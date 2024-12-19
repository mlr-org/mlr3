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
#' The text in brackets indicates what happens between the stages and which accesses to the [ContextEvaluation] (`ctx`) are typical for the stage.
#'
#' ```
#' Start Resampling Iteration on Worker
#'  - on_evaluation_begin
#'    (Split `ctx$task` into training and test set with `ctx$resampling` and `ctx$iteration`)
#'  - on_evaluation_before_train
#'    (Train the learner `ctx$learner` on training data)
#'  - on_evaluation_before_predict
#'    (Predict on predict sets and store prediction data `ctx$pdatas`)
#'  - on_evaluation_end
#'    (Erase model `ctx$learner$model` if requested and return results)
#' End Resampling Iteration on Worker
#' ```
#'
#' The callback can store data in `ctx$learner$state` or `ctx$data_extra`.
#' The data in `ctx$data_extra` is stored in the [ResampleResult] or [BenchmarkResult].
#' See also the section on parameters for more information on the stages.
#
#' @details
#' When implementing a callback, each function must have two arguments named `callback` and `context`.
#' A callback can write data to the state (`$state`), e.g. settings that affect the callback itself.
#'
#' @section Parallelization:
#' Be careful when modifying `ctx$learner`, `ctx$task`, or `ctx$resampling` because callbacks can behave differently when parallelizing the resampling process.
#' When running the resampling process sequentially, the modifications are carried over to the next iteration.
#' When parallelizing the resampling process, modifying the [ContextEvaluation] will not be synchronized between workers.
#' This also applies to the `$state` of the callback.
#'
#' @param id (`character(1)`)\cr
#'   Identifier for the new instance.
#' @param label (`character(1)`)\cr
#'   Label for the new instance.
#' @param man (`character(1)`)\cr
#'   String in the format `[pkg]::[topic]` pointing to a manual page for this object.
#'   The referenced help package can be opened via method `$help()`.
#' @param on_evaluation_begin (`function()`)\cr
#'   Stage called at the beginning of an evaluation.
#'   Called in `workhorse()` (internal).
#' @param on_evaluation_before_train (`function()`)\cr
#'   Stage called before training the learner.
#'   Called in `workhorse()` (internal).
#' @param on_evaluation_before_predict (`function()`)\cr
#'   Stage called before predicting.
#'   Called in `workhorse()` (internal).
#' @param on_evaluation_end (`function()`)\cr
#'   Stage called at the end of an evaluation.
#'   Called in `workhorse()` (internal).
#'
#' @export
#' @examples
#' callback = callback_evaluation("selected_features",
#'  label = "Selected Features",
#'
#'  on_evaluation_end = function(callback, context) {
#'     context$learner$state$selected_features = context$learner$selected_features()
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
  assert_class(callback, "CallbackEvaluation", null.ok = null_ok)
  invisible(callback)
}

#' @export
#' @param callbacks (list of [CallbackEvaluation]).
#' @rdname assert_evaluation_callback
assert_evaluation_callbacks = function(callbacks, null_ok = FALSE) {
  assert_list(callbacks, null.ok = null_ok)
  if (null_ok && is.null(callbacks)) return(invisible(NULL))
  invisible(lapply(callbacks, assert_evaluation_callback))
}

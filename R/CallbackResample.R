#' @title Resample Callback
#'
#' @description
#' Specialized [mlr3misc::Callback] to customize the behavior of [resample()] and [benchmark()] in mlr3.
#' For example, callbacks can be used to extract information from models on the worker or to store intermediate results to disk.
#' The [callback_resample()] function is used to create instances of this class.
#' Predefined callbacks are stored in the [dictionary][mlr3misc::Dictionary] [mlr_callbacks] and can be retrieved with [clbk()].
#' For more information on callbacks, see the [callback_resample()] documentation.
#'
#' @export
CallbackResample = R6Class("CallbackResample",
  inherit = Callback,
  public = list(

    #' @field on_resample_begin (`function()`)\cr
    #' Stage called at the beginning of the resampling iteration.
    #' Called in `workhorse()` (internal).
    on_resample_begin = NULL,

    #' @field on_resample_before_train (`function()`)\cr
    #' Stage called before training the learner.
    #' Called in `workhorse()` (internal).
    on_resample_before_train = NULL,

    #' @field on_resample_before_predict (`function()`)\cr
    #' Stage called before predicting.
    #' Called in `workhorse()` (internal).
    on_resample_before_predict = NULL,

    #' @field on_resample_end (`function()`)\cr
    #' Stage called at the end of the resample iteration.
    #' Called in `workhorse()` (internal).
    on_resample_end = NULL
  )
)

#' @title Create Evaluation Callback
#'
#' @description
#' Function to create a [CallbackResample].
#' Predefined callbacks are stored in the [dictionary][mlr3misc::Dictionary] [mlr_callbacks] and can be retrieved with [clbk()].
#'
#' Evaluation callbacks are called at different stages of the resampling process.
#' Each stage is called once per resampling iteration.
#' The stages are prefixed with `on_resample_*`.
#' The text in brackets indicates what happens between the stages in the internal `workhorse()` function and which accesses to the [ContextResample] (`ctx`) are typical for the stage.
#'
#' ```
#' Start Resampling Iteration on Worker
#'  - on_resample_begin
#'    (Split `ctx$task` into training and test set with `ctx$resampling` and `ctx$iteration`)
#'  - on_resample_before_train
#'    (Train the learner `ctx$learner` on training data)
#'  - on_resample_before_predict
#'    (Predict on predict sets and store prediction data `ctx$pdatas`)
#'  - on_resample_end
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
#' We highly discourage changing the task, learner and resampling objects via the callback.
#'
#' @param id (`character(1)`)\cr
#'   Identifier for the new instance.
#' @param label (`character(1)`)\cr
#'   Label for the new instance.
#' @param man (`character(1)`)\cr
#'   String in the format `[pkg]::[topic]` pointing to a manual page for this object.
#'   The referenced help package can be opened via method `$help()`.
#' @param on_resample_begin (`function()`)\cr
#'   Stage called at the beginning of an evaluation.
#'   Called in `workhorse()` (internal).
#' @param on_resample_before_train (`function()`)\cr
#'   Stage called before training the learner.
#'   Called in `workhorse()` (internal).
#' @param on_resample_before_predict (`function()`)\cr
#'   Stage called before predicting.
#'   Called in `workhorse()` (internal).
#' @param on_resample_end (`function()`)\cr
#'   Stage called at the end of an evaluation.
#'   Called in `workhorse()` (internal).
#'
#' @export
#' @examples
#' learner = lrn("classif.rpart")
#' task = tsk("pima")
#' resampling = rsmp("cv", folds = 3)
#'
#' # save selected features callback
#' callback = callback_resample("selected_features",
#'  on_resample_end = function(callback, context) {
#'     context$learner$state$selected_features = context$learner$selected_features()
#'   }
#' )
#'
#' rr = resample(task, learner, resampling, callbacks = callback)
#' rr$learners[[1]]$state$selected_features
#'
#' # holdout task callback
#' callback = callback_resample("holdout_task",
#'   on_resample_before_predict = function(callback, context) {
#'     pred = context$learner$predict(callback$state$task)
#'     context$data_extra = list(prediction_holdout = pred)
#'   }
#' )
#'
#' task_holdout = tsk("pima")
#' splits = partition(task, 0.7)
#' task$filter(splits$train)
#' task_holdout$filter(splits$test)
#'
#' callback$state$task = task_holdout
#'
#' rr = resample(task, learner, resampling, callbacks = callback)
#' rr$data_extra
callback_resample = function(
  id,
  label = NA_character_,
  man = NA_character_,
  on_resample_begin = NULL,
  on_resample_before_train = NULL,
  on_resample_before_predict = NULL,
  on_resample_end = NULL
  ) {
  stages = discard(set_names(list(
    on_resample_begin,
    on_resample_before_train,
    on_resample_before_predict,
    on_resample_end),
    c(
      "on_resample_begin",
      "on_resample_before_train",
      "on_resample_before_predict",
      "on_resample_end"
    )), is.null)

  stages = map(stages, function(stage) crate(assert_function(stage, args = c("callback", "context"))))
  callback = CallbackResample$new(id, label, man)
  iwalk(stages, function(stage, name) callback[[name]] = stage)
  callback
}

#' @title Assertions for Callbacks
#'
#' @description
#' Assertions for [CallbackResample] class.
#'
#' @param callback ([CallbackResample]).
#' @param null_ok (`logical(1)`)\cr
#'   If `TRUE`, `NULL` is allowed.
#'
#' @return [CallbackResample] | List of [CallbackResample]s.
#' @export
assert_resample_callback = function(callback, null_ok = FALSE) {
  assert_class(callback, "CallbackResample", null.ok = null_ok)
  invisible(callback)
}

#' @export
#' @param callbacks (list of [CallbackResample]).
#' @rdname assert_resample_callback
assert_resample_callbacks = function(callbacks, null_ok = FALSE) {
  assert_list(callbacks, null.ok = null_ok)
  if (null_ok && is.null(callbacks)) return(invisible(NULL))
  invisible(lapply(callbacks, assert_resample_callback))
}

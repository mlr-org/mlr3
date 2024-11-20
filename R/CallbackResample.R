#' @title Create Batch Tuning Callback
#'
#' @description
#' Specialized [bbotk::CallbackBatch] for batch tuning.
#' Callbacks allow to customize the behavior of processes in mlr3tuning.
#' The [callback_batch_tuning()] function creates a [CallbackResample].
#' Predefined callbacks are stored in the [dictionary][mlr3misc::Dictionary] [mlr_callbacks] and can be retrieved with [clbk()].
#' For more information on tuning callbacks see [callback_batch_tuning()].
#'
#' @export
CallbackResample= R6Class("CallbackResample",
  inherit = Callback,
  public = list(

    #' @field on_resample_before_result_data (`function()`)\cr
    #' Stage called before the result data is created.
    on_resample_before_result_data = NULL
  )
)

#' @title Create Batch Tuning Callback
#'
#' @description
#' Function to create a [CallbackResample].
#' Predefined callbacks are stored in the [dictionary][mlr3misc::Dictionary] [mlr_callbacks] and can be retrieved with [clbk()].
#'
#' Tuning callbacks can be called from different stages of the tuning process.
#' The stages are prefixed with `on_*`.
#'
#' ```
#' Start Tuning
#'      - on_optimization_begin
#'     Start Tuner Batch
#'          - on_optimizer_before_eval
#'         Start Evaluation
#'              - on_eval_after_design
#'              - on_eval_after_benchmark
#'              - on_eval_before_archive
#'         End Evaluation
#'          - on_optimizer_after_eval
#'     End Tuner Batch
#'      - on_tuning_result_begin
#'      - on_result_begin
#'      - on_result_end
#'      - on_optimization_end
#' End Tuning
#' ```
#'
#' See also the section on parameters for more information on the stages.
#' A tuning callback works with [ContextBatchTuning].
#'
#' @details
#' When implementing a callback, each function must have two arguments named `callback` and `context`.
#' A callback can write data to the state (`$state`), e.g. settings that affect the callback itself.
#' Tuning callbacks access [ContextBatchTuning].
#'
#' @param id (`character(1)`)\cr
#'  Identifier for the new instance.
#' @param label (`character(1)`)\cr
#'  Label for the new instance.
#' @param man (`character(1)`)\cr
#'  String in the format `[pkg]::[topic]` pointing to a manual page for this object.
#'  The referenced help package can be opened via method `$help()`.
#'
#' @param on_optimization_begin (`function()`)\cr
#'  Stage called at the beginning of the optimization.
#'  Called in `Optimizer$optimize()`.
#'  The functions must have two arguments named `callback` and `context`.
#'
#' @export
#' @inherit CallbackResample examples
callback_resample = function(
  id,
  label = NA_character_,
  man = NA_character_,
  on_resample_before_result_data = NULL
  ) {
  stages = discard(set_names(list(
    on_resample_before_result_data),
    c(
      "on_resample_before_result_data"
      )), is.null)

  walk(stages, function(stage) assert_function(stage, args = c("callback", "context")))
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
#' @return [CallbackResample | List of [CallbackResample]s.
#' @export
assert_resample_callback = function(callback, null_ok = FALSE) {
  if (null_ok && is.null(callback)) return(invisible(NULL))
  browser()
  assert_class(callback, "CallbackResample")
  invisible(callback)
}

#' @export
#' @param callbacks (list of [CallbackResample]).
#' @rdname assert_resample_callback
assert_resample_callbacks = function(callbacks) {
  invisible(lapply(callbacks, assert_resample_callback))
}

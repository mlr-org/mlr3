#' @title Batch Tuning Context
#'
#' @description
#' A [CallbackResample] accesses and modifies data during the optimization via the `ContextBatchTuning`.
#' See the section on active bindings for a list of modifiable objects.
#' See [callback_batch_tuning()] for a list of stages that access `ContextBatchTuning`.
#'
#' @template param_inst_batch
#' @template param_tuner
#'
#' @export
ContextResample = R6Class("ContextResample",
  inherit = Context,
  public = list(
    data = NULL
  )
)

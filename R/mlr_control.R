#' @title Execution Control Object
#'
#' @description
#' This function creates a named list of settings which control the execution of a [Learner].
#'
#' * `store_model` (`logical(1))`:\cr
#'   If `FALSE`, the model fitted during `train()` is discarded after `predict()`.
#'   Note that you will be unable to predict on new data afterwards.
#' * `encapsulate_train` (`character(1)`):\cr
#'   How to call external code in third party packages during train.
#'     - If set to `"none"` (default), the code is executed in the running session without error handling.
#'       Output is not stored, just send to the console.
#'     - If set to `"evaluate"`, the exceptions are caught using [evaluate::evaluate()].
#'       All output can be accessed via the learner field `$log`.
#'       \CRANpkg{evaluate} does not start a separate session, and thus cannot guard you against segfaults.
#'     - If set to `"callr"`, the code is executed in an independent R session using the \CRANpkg{callr} package.
#'       All output can be accessed via the learner field `$log`.
#'       This guards your session from segfaults, at the cost of some computational overhead.
#' * `encapsulate_predict` (`character(1)`):\cr
#'   How to call external code in third party packages during predict. Same format as `encapsulate_train`.
#'
#' **Defaults**
#' ```
#' $store_model
#' TRUE
#'
#' $encapsulate_train
#' "none"
#'
#' $encapsulate_predict
#' "none"
#'
#' $log_threshold
#' 400
#' ```
#'
#' @param ... :: any\cr
#'   Named arguments to overwrite the defaults / options.
#'   Settings may be provided in a `name = value` fashion, or by providing a single named `list()`.
#'
#' @return (named `list()`) of all settings.
#'
#' @export
#' @examples
#' # get a list of the defaults
#' mlr_control()
mlr_control = function(...) {

  ctrl = mlr_reflections$mlr_control_defaults
  ctrl$log_threshold = lg$threshold

  ldots = ...length()
  if (ldots == 0L) {
    return(ctrl)
  }

  dots = list(...)
  if (ldots == 1L && is.null(names(dots)) && is.list(dots[[1L]])) {
    dots = dots[[1L]]
    if (length(dots) == 0L) {
      return(ctrl)
    }
  }

  assert_names(names(dots), "unique")
  ii = wf(names(dots) %nin% names(ctrl))
  if (length(ii)) {
    stopf("Unknown option '%s'!%s", names(dots)[ii], did_you_mean(names(dots)[ii], names(ctrl)))
  }
  assert_flag(dots$store_model, null.ok = TRUE)
  assert_choice(dots$encapsulate_train, c("none", "evaluate", "callr"), null.ok = TRUE)
  assert_choice(dots$encapsulate_predict, c("none", "evaluate", "callr"), null.ok = TRUE)
  ctrl[names(dots)] = dots
  ctrl
}

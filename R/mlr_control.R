#' @title Execution Control Object
#'
#' @description
#' This function creates a named list of settings which control the execution of an [Experiment].
#'
#' * `store_model`: If `FALSE`, the model returned by the learner is discarded in order to save some memory after the experiment is completed.
#'  Note that you will be unable to further predict on new data.
#' * `store_prediction`: If `FALSE`, the predictions are discarded in order to save some memory after the experiment is completed.
#' * `encapsulate_train`: How to call external code in third party packages during train.
#'     - If set to `"none"` (default), the code is executed in the running session without error handling.
#'       Output is not stored, just send to the console.
#'     - If set to `"evaluate"`, the exceptions are caught using [evaluate::evaluate()].
#'       All output is stored in a [Log] of the corresponding [Experiment].
#'       \CRANpkg{evaluate} does not start a separate session, and thus cannot guard you against segfaults.
#'     - If set to `"callr"`, the code is executed in an independent R session using the \CRANpkg{callr} package.
#'       All output is stored in a [Log] of the corresponding [Experiment].
#'       This guards your session from segfaults, at the cost of some computational overhead.
#'   See [Log] for an example.
#'
#' * `encapsulate_predict`: How to call external code in third party packages during predict.
#'   Same format as `encapsulate_train`. See [Log] for an example.
#'
#' @param ... Named arguments to overwrite the defaults / options.
#'  Settings may be provided in a `name = value` fashion, or by providing a single named `list()`.
#'
#' @return (named `list()`) of all settings.
#'
#' @export
#' @examples
#' # get a list of the defaults
#' mlr_control()
#'
#' # get a control object, with the default of store_model changed to FALSE
#' mlr_control(store_model = FALSE)
mlr_control = function(...) {

  ctrl = default_mlr_control
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
  ctrl[names(dots)] = dots
  ctrl
}

default_mlr_control = list(
  store_model = TRUE,
  store_prediction = TRUE,
  encapsulate_train = "none",
  encapsulate_predict = "none",
  log_threshold = 400L
)

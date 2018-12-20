#' @title Execution Control Object
#' @description
#' This function creates a named list of settings which control the execution of an [Experiment].
#'
#' * `store_model`: If `FALSE`, the model returned by the learner is discarded in order to save some memory after the experiment is completed.
#'  Note that you will be unable to further predict on new data.
#' * `store_prediction`: If `FALSE`, the predictions are discarded in order to save some memory after the experiment is completed.
#' * `encapsulate_train`: How to call external code in third party packages during train.
#'     - If set to `"none"` (default), the code is executed in the running session without error handling.
#'       Output is not stored, just send to the console.
#'     - If set to `"evaluate"`, the exceptions are caught using [evaluate::evaluate()], and output is stored in a [Log] of the corresponding [Experiment].
#'     - If set to `"callr"`, the code is executed in an independent R session. This guards your session from segfaults,
#'       at the cost of some computational overhead. Logs are also stored in the [Experiment].
#' * `encapsulate_predict`: How to call external code in third party packages during predict.
#'   Same format as `encapsulate_train`.
#'
#' @param ... Named arguments to overwrite the defaults / options.
#'
#' @return (named `list()`). If no argument is provided, returns all settings as named list.
#'   If arguments are provided in a `name = value` fashion, the settings are returned as named list
#'   after some argument checks.
#'
#' @export
#' @references [HTML help page](https://mlr3.mlr-org.com/reference/mlr_control.html)
#' @examples
#' # get a list of the defaults
#' mlr_control()
#'
#' # get a control object, with the default of store_model switched to FALSE
#' mlr_control(store_model = FALSE)
mlr_control = function(...) {
  ctrl = mlr_reflections$default_mlr_control
  ctrl$log_threshold = logger::log_threshold(namespace = "mlr3")

  ldots = ...length()
  if (ldots == 0L)
    return(ctrl)

  dots = list(...)
  if (ldots == 1L && is.null(names(dots)) && is.list(dots[[1L]])) {
    dots = dots[[1L]]
    if (length(dots) == 0L)
      return(ctrl)
  }

  assert_names(names(dots), "unique")
  ii = wf(names(dots) %nin% names(ctrl))
  if (length(ii))
    stopf("Unknown option '%s'!%s", names(dots)[ii], did_you_mean(names(dots)[ii], names(ctrl)))
  ctrl[names(dots)] = dots
  ctrl
}

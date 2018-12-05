#' @title Execution control object
#'
#' @description
#' This function creates a named list of settings which control the execution of an [Experiment].
#'
#' * `store_model`: If `FALSE`, the model returned by the learner is discarded in order to save some memory after the experiment is completed.
#'  Note that you will be unable to further predict on new data.
#' * `store_prediction`: If `FALSE`, the predictions are discarded in order to save some memory after the experiment is completed.
#' * `use_evaluate`: Set to `TRUE` to capture output via \pkg{evaluate} and store it as log.
#' * `use_future`: Set to `FALSE` to disable parallelization via \pkg{future}. This sometimes simplifies debugging.
#'
#' @param ... Named arguments to overwrite the defaults / options.
#'
#' @return (named `list()`). If no argument is provided, returns all settings as named list.
#'   If arguments are provided in a `name = value` fashion, the settings are returned as named list
#'   after some argument checks.
#'
#' @export
#' @examples
#' # get a list of the defaults
#' mlr_control()
#'
#' # get a control object, change default of store_model
#' mlr_control(store_model = FALSE)
mlr_control = function(...) {
  ctrl = mlr_reflections$default_mlr_control
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

use_future = function(ctrl) {
  isTRUE(ctrl$use_future) &&
    requireNamespace("future", quietly = TRUE) &&
    requireNamespace("future.apply", quietly = TRUE)
}

use_evaluate = function(ctrl) {
  if (ctrl$use_evaluate)
    require_namespaces("evaluate")
  ctrl$use_evaluate
}

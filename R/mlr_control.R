#' @title Execution Control Object
#'
#' @description
#' This function creates a named list of settings which control the execution of a [Learner].
#'
#' * `store_models` (`logical(1))`:\cr
#'   If `FALSE` (default), models fitted during [resample()] and [benchmark()] are discarded.
#'   Note that you will be unable to predict on new data or extract additional information like
#'   variable importance from the learner.
#'
#' * `log_threshold` (`integer(1)`):\cr The verbosity of logging output. Numeric
#' values correspond to different verbosity levels. See [the lgr
#' vignette](https://s-fleck.github.io/lgr/articles/lgr.html#log-levels) for
#' more information. A log-level dictionary is available with
#' `getOption("lgr.log_levels")`. To change the log level for _mlr3_ either use
#' `lgr::get_logger("mlr3")$set_threshold("<level>")` or set it globally via
#' `options("lgr.default_threshold")`.
#'
#'
#' **Defaults**
#' ```
#' $store_models
#' TRUE
#'
#' $log_threshold
#' 400
#' ```
#'
#' @param ... :: `any`\cr
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
  assert_flag(dots$store_models, null.ok = TRUE)
  ctrl[names(dots)] = dots
  ctrl
}

#' @title Execution control object
#'
#' @description
#' This function creates a named list of settings which control the execution of an [Experiment].
#' It contains all options (see [mlr_options()]) without the `"mlr3"` prefix, and additionally:
#'
#' * `store_model`: If `FALSE`, the model returned by the learner is discarded in order to save some memory after the experiment is completed.
#'  Note that you will be unable to further predict on new data.
#' * `store_prediction`: If `FALSE`, the predictions are discarded in order to save some memory after the experiment is completed.
#' * `use_evaluate`: Capture output via \pkg{evaluate} and store it as log.
#'
#' @param ... Named arguments to overwrite the defaults / options.
#'
#' @export
#' @examples
#' # get a list of the currently active defaults
#' mlr_control()
#'
#' # enable debuging
#' mlr_control(debug = TRUE)
mlr_control = function(...) {
  opts = mlr_options()
  names(opts) = substr(names(opts), 6L, 255L)
  ctrl = insert_named(mlr_reflections$default_mlr_control, opts)

  dots = list(...)
  if (length(dots) > 0L) {
    if (length(dots) == 1L && is.null(names(dots)) && is.list(dots[[1L]]))
      dots = dots[[1L]]
    assert_names(names(dots), "unique")

    ii = wf(names(dots) %nin% names(ctrl))
    if (length(ii))
      stopf("Unknown option '%s'!%s", names(dots)[ii], did_you_mean(names(dots)[ii], names(ctrl)))
    ctrl = insert_named(ctrl, dots)
  }

  ctrl
}

use_future = function(ctrl = NULL) {
  opt = if (is.null(ctrl)) getOption("mlr3.use_future") else ctrl$use_future
  isTRUE(opt) && requireNamespace("future", quietly = TRUE) && requireNamespace("future.apply", quietly = TRUE)
}

use_evaluate = function(ctrl) {
  ctrl$use_evaluate && requireNamespace("evaluate", quietly = TRUE)
}

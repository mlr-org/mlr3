#' @title Execution control object
#'
#' @description
#' This function creates a named list of settings which control the execution of an [Experiment].
#' It contains all options (see [mlr_options()]) without the `"mlr3"` prefix, and additionally:
#'
#' * `store_model`: If `FALSE`, the model returned by the learner is discarded in order to save some memory after the experiment is completed.
#'  Note that you will be unable to further predict on new data.
#' * `store_prediction`: If `FALSE`, the predictions are discarded in order to save some memory after the experiment is completed.
#'  Note that you will be unable calculate more performance measures.
#' * `error_handling`: How to deal with models raising exceptions during `train` or `predict`?
#'   - `"off"` (default). An exception is raised, stopping the execution.
#'   - `"catch"`. Exceptions are caught and logged. There will be no predictions available, and the performance will be `NA`.
#'     All output is stored in the [Experiment] as a [Log].
#'   - `"impute_worst"`. This is similar to the `"catch"` approach, but instead of predicting `NA`, the worst
#'     possible performance is predicted.
#'   - `"fallback_train"`. If the learner fails to fit a model during train, fit a fallback model, e.g. with a featureless learner.
#'     The fallback learner is in this case used to generate predictions which are then scored.
#'     Note that this mechanism does not guard you from models which successfully train, but raise exceptions during predict.
#'     This would result in missing predictions and `NA` scores.
#'   - `"fallback"`. Always fit a fallback model and use it if the learner fails to train or predict.
#' * `fallback_learner`: If `"error_handling"` is set to `"fallback_train"` or `"fallback"`, use this learner as fallback learner.
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
  ec = insert_named(mlr_reflections$default_mlr_control, mlr_options())
  if (...length()) {
    opts = assert_list(list(...), names = "unique")

    ii = wf(names(opts) %nin% names(ec))
    if (length(ii))
      stopf("Unknown option '%s'!%s", names(opts)[ii], did_you_mean(names(opts)[ii], names(ec)))

    ec = insert_named(ec, opts)
  }
  ec
}

use_future = function(ctrl = NULL) {
  opt = if (is.null(ctrl)) getOption("mlr3.use_future") else ctrl$use_future
  isTRUE(opt) && requireNamespace("future", quietly = TRUE) && requireNamespace("future.apply", quietly = TRUE)
}

use_evaluate = function(ctrl = NULL) {
  opt = if (is.null(ctrl)) getOption("mlr3.error_handling") else ctrl$error_handling
  assert_choice(opt, c("off", "catch", "impute_worst", "fallback_train", "fallback"), .var.name = "Option 'error_handling'")
  if (opt == "off")
    return(FALSE)
  require_namespaces("evaluate")
  return(TRUE)
}

default_exec_control = list(
  store_model = TRUE
)

#' @title Execution control object
#'
#' @description
#' This function creates a named list of settings which control the execution of an [Experiment].
#' It contains all options (without the `"mlr3"` prefix) and additionally:
#'
#' * `store_model`: If `FALSE`, the model returned by the learner is discarded in order to save some memory after the experiment is completed.
#'  Note that you will be unable to further investigate the experiment or predict on new data.
#'
#' @param ... Named arguments to overwrite the defaults / options.
#'
#' @export
#' @examples
#' # get a list of the currently active defaults
#' exec_control()
#'
#' # enable debuging
#' exec_control(debug = TRUE)
exec_control = function(...) {
  ec = insert(default_exec_control, mlr_options())
  if (...length()) {
    opts = assert_list(list(...), names = "unique")

    ii = wf(names(opts) %nin% names(ec))
    if (length(ii)) {
      suggested = stri_suggest(names(opts)[ii], names(ec))
      suggested = if (length(suggested) == 0L) "" else sprintf(" Did you mean: %s?", paste0(suggested, collapse = " / "))
      stopf("Unknown option '%s'. %s", names(opts)[ii], suggested)
    }

    ec = insert(ec, opts)
  }
  ec
}

use_future = function(ctrl = NULL) {
  opt = if (is.null(ctrl)) getOption("mlr3.use_future") else ctrl$use_future
  isTRUE(opt) && requireNamespace("future", quietly = TRUE) && requireNamespace("future.apply", quietly = TRUE)
}

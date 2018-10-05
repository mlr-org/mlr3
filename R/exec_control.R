default_exec_control = list(
  discard.model = FALSE
)

#' @title Execution control object
#'
#' @description
#' This function creates a named list of settings which control the execution of an [Experiment].
#' It contains all options (without the `"mlr3"` prefix) and additionally:
#' 
#' * `discard.model`: If `TRUE`, the model as returned by the learner is discarded after the experiment is finished.
#'  Set this to save some memory. Note that you will be unable to further investigate the experiment or #'  predict on new data.
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
      # TODO: write a helper function for this
      suggested = stri_suggest(names(opts)[ii], names(ec))
      suggested = if (length(suggested) == 0L) "" else sprintf(" Did you mean: %s?", paste0(suggested, collapse = " / "))
      stopf("Unknown option '%s'. %s", names(opts)[ii], suggested)
    }

    ec = insert(ec, opts)
  }
  ec
}

use_future = function(ctrl = NULL) {
  opt = if (is.null(ctrl)) ctrl$use.future else getOption("mlr3.use.future")
  isTRUE(opt) && requireNamespace("future", quietly = TRUE) && requireNamespace("future.apply", quietly = TRUE)
}

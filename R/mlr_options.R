#' @title Options for mlr3
#'
#' @description
#' The following options are currently supported by \pkg{mlr3}:
#'
#' * `mlr3.verbose`: Set to `FALSE` to suppress output of informational messages.
#' * `mlr3.debug`: Set to `TRUE` to enable extra output and additional (potentially expensive) assertions.
#' * `mlr3.use_future`: Set to `FALSE` to disable parallelization via \pkg{future}.
#'   Explicitly disabling this simplifies debugging.
#'
#' @export
#' @examples
#' # get a list of currently set options (without the prefix)
#' mlr_options()
mlr_options = function() {
  opts = .Options[startsWith(names(.Options), "mlr3.")]
  names(opts) = substr(names(opts), 6L, 100L)
  opts
}


#' @title Options for mlr3
#'
#' @description
#' The following options are currently supported by \pkg{mlr3}:
#'
#' * `mlr3.verbose`: Set to `FALSE` to suppress output of informational messages.
#' * `mlr3.debug`: Set to `TRUE` to enable extra output and additional (potentially expensive) assertions.
#'
#' @param ... : Named arguments to set. Alternatively, you can also provide a single list of named arguments
#'   (similar to [options()]).
#'
#' @return (named `list`). Currently set options specific to mlr3.
#'
#' @export
#' @examples
#' # get a list of currently set options (without the prefix)
#' mlr_options()
#' old = mlr_options(mlr3.debug = TRUE)
#' mlr_options(old)
mlr_options = function(...) {
  opts = .Options[startsWith(names(.Options), "mlr3.")]

  dots = list(...)
  if (length(dots) > 0L) {
    if (length(dots) == 1L && is.null(names(dots)) && is.list(dots[[1L]]))
      dots = dots[[1L]]
    assert_names(names(dots), "unique")

    if (!all(startsWith(names(dots), "mlr3.")))
      stopf("Options need to start with 'mlr3.'")
    options(dots)
  } else {
    opts
  }
}

mlr_reflections$default_mlr_options = list(
  mlr3.verbose = TRUE,
  mlr3.debug = FALSE
)



  # Set default options without overwriting already set options
  opts = mlr_reflections$default_mlr_options
  opts = opts[match(names(opts), names(.Options), nomatch = 0L) == 0L]
  if (length(opts))
    options(opts)

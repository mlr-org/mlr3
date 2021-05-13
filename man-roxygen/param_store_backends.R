#' @param store_backends (`logical(1)`)\cr
#'   Keep the [DataBackend] of the [Task] in the [ResampleResult]?
#'   Set to `TRUE` if your performance measures require a [Task],
#'   or to analyse results more conveniently.
#'   Set to `FALSE` to reduce the file size and memory footprint
#'   after serialization.
#'   The current default is `TRUE`, but this eventually will be changed
#'   in a future release.

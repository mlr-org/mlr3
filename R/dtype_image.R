#' @param x (`character`) \cr
#'   Vector of (relative) image paths
#' @export
as.imagepath = function(x) {
  assert_character(x, pattern = "([a-z_\\-\\s0-9\\.~]+)\\.([A-Za-z0-9_-])+$")
  structure(x, class = "imagepath")
}

#' @param x (`character`) \cr
#'   Vector of (relative) image paths
#' @rdname mlr_assertions
#' @export
assert_imagepath = function(x) {
  assert_character(x, pattern = "([a-z_\\-\\s0-9\\.~]+)\\.([A-Za-z0-9_-])+$")
}

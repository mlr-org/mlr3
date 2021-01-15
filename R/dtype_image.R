#' @param x (`character`) \cr
#'   Vector of (relative) image paths
#' @export
as.imagepath = function(x) {
  assert_character(x)
  structure(x, class = "imagepath")
}

#' @param x (`character`) \cr
#'   Vector of (relative) image paths
#' @rdname mlr_assertions
#' @export
assert_imagepath = function(x) {
  assert_character(x)
}

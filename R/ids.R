#' @title Extract the id slot
#'
#' @description
#' A simple helper which extracts the id slot of a list of objects.
#'
#' @param x (`list`)\cr
#'  List of objects which have a slot `"id"`.
#'
#' @return (`character`) of the same length as input `x`.
#' @export
#' @examples
#' x = mlr_tasks$mget(c("iris", "sonar"))
#' ids(x)
ids = function(x) {
  vcapply(x, "[[", "id", use.names = FALSE)
}


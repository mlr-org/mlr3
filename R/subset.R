#' @title Declare type of row subsetting
#' @name subsetting-types
#' @rdname subsetting-types
#'
#' @description
#' These functions can be used to switch between different ways of declaring subsets of task observations.
#' \code{row_ids} is used to declare direct usage of \dQuote{row_ids} corresponding to \code{task$row_info}.
#' \code{row_index} allows indexing with integer values, and \code{row_roles} declares to use rows with matching roles (as in \code{task$row_info}).
#'
#' @param x (`[atomic vector][base::atomic()]`)\cr
#'   Row declaration, depending on the function.
#' @return (`[atomic vector][base::atomic()]`) with added attribute \dQuote{subset_type}.
#' @export
#' @examples
#' task = mlr_tasks$get("iris")
#' lrn = mlr_learners$get("classif.rpart")
#'
#' # List row_ids:
#' task$row_info
#'
#' # Mark each third observation so that it is not used per default for training:
#' task$row_info[seq_len(task$nrow) %% 3 == 0, role := "ignore"]
#' task$nrow
#'
#' # Use all with role == "training":
#' train(task, lrn)$train_set
#'
#' # Same, but more explicitly:
#' train(task, lrn, subset = row_roles("training"))$train_set
#'
#' # Train on rows with role == "training", indexed by number and ordered as in task$row_info
#' train(task, lrn, subset = row_index(1:80))$train_set
#'
#' # Use specific row_ids:
#' train(task, lrn, subset = row_ids(c(1, 2, 52, 53, 101, 103)))$train_set
#'
#' # Note that you can also use ids with different roles here:
#' train(task, lrn, subset = row_ids(1:2))$train_set
row_ids = function(x) {
  assert_atomic_vector(x, any.missing = FALSE)
  attr(x, "subset_type") = "ids"
  x
}

#' @rdname subsetting-types
#' @export
row_index = function(x) {
  x = asInteger(x, any.missing = FALSE)
  attr(x, "subset_type") = "numbers"
  x
}

#' @rdname subsetting-types
#' @export
row_roles = function(x) {
  assert_character(x, min.len = 1L, any.missing = FALSE)
  attr(x, "subset_type") = "roles"
  x
}

assert_row_ids = function(x) {
  type = attr(x, "subset_type") %??% "unknown"
  if (!identical(type, "ids"))
    stopf("Ids must be provided explicitly as row_ids, not as '%s'", type)
  invisible(x)
}

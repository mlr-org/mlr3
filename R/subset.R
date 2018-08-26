#' @title Declare type of row subsetting
#' @name subsetting-types
#' @rdname subsetting-types
#'
#' @description
#' These functions can be used to switch between different ways of declaring subsets of task observations.
#' \code{row.ids} is used to declare direct usage of \dQuote{row.ids} corresponding to \code{task$rows}.
#' \code{row.index} allows indexing with integer values, and \code{row.roles} declares to use rows with matching roles (as in \code{task$rows}).
#'
#' @param x [\code{atomic vector}]\cr
#'   Row declaration, depending on the function.
#' @return [\code{atomic vector}] with added attribute \dQuote{subset.type}.
#' @export
#' @examples
#' task = mlr.tasks$get("iris")
#' lrn = mlr.learners$get("classif.rpart")
#'
#' # List row ids:
#' task$rows
#'
#' # Mark each third observation so that it is not used per default for training:
#' task$rows[seq_len(task$nrow) %% 3 == 0, role := "ignore"]
#' task$nrow
#'
#' # Use all with role == "training":
#' train(task, lrn)$train.set
#'
#' # Same, but more explicitly:
#' train(task, lrn, subset = row.roles("training"))$train.set
#'
#' # Train on the ignored rows:
#' train(task, lrn, subset = row.roles("ignore"))$train.set
#'
#' # Train on rows with role == "training", indexed by number and ordered as in task$rows
#' train(task, lrn, subset = row.index(1:80))$train.set
#'
#' # Use specific row ids:
#' train(task, lrn, subset = row.ids(c(1, 2, 52, 53, 101, 103)))$train.set
#'
#' # Note that you can also use ids with different roles here:
#' train(task, lrn, subset = row.ids(1:150))$train.set
row.ids = function(x) {
  assertAtomicVector(x, any.missing = FALSE)
  attr(x, "subset.type") = "ids"
  x
}

#' @rdname subsetting-types
#' @export
row.index = function(x) {
  x = asInteger(x, any.missing = FALSE)
  attr(x, "subset.type") = "numbers"
  x
}

#' @rdname subsetting-types
#' @export
row.roles = function(x) {
  assertCharacter(x, min.len = 1L, any.missing = FALSE)
  attr(x, "subset.type") = "roles"
  x
}

assertRowIds = function(x) {
  type = attr(x, "subset.type") %??% "unknown"
  if (!identical(type, "ids"))
    stopf("Ids must be provided explicitly as row ids, not as '%s'", type)
  invisible(x)
}

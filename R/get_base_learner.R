#' @title Extract Base Learner
#'
#' @description
#' Extracts the base learners from nested objects such as the
#' `GraphLearner` in \CRANpkg{mlr3pipelines}.
#' Returns the [Learner] itself for regular learners,
#' and `NULL` if there is no learner to extract.
#'
#' @param x (any)\cr
#'   Object to extract the base learner from.
#' @param ... (any)\cr
#'   Currently not used.
#'
#' @return [Learner] or `NULL`.
#' @keywords internal
#' @export
get_base_learner = function(x, ...) {
  UseMethod("get_base_learner")
}

#' @export
get_base_learner.default = function(x, ...) { # nolint
  NULL
}

#' @export
get_base_learner.Learner = function(x, ...) { # nolint
  x
}

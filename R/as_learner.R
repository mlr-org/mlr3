#' @title Convert to a Learner
#'
#' @description
#' Convert object to a [Learner] or a list of [Learner].
#'
#' @inheritParams as_task
#' @return [Learner].
#' @export
as_learner = function(x, ...) { # nolint
  UseMethod("as_learner")
}

#' @export
#' @rdname as_learner
as_learner.Learner = function(x, clone = FALSE, ...) { # nolint
  if (clone) x$clone(deep = TRUE) else x
}

#' @export
#' @rdname as_learner
as_learners = function(x, clone = FALSE, ...) { # nolint
  UseMethod("as_learners")
}

#' @export
#' @rdname as_learner
as_learners.list = function(x, clone = FALSE, ...) { # nolint
  lapply(x, as_learner, clone = clone, ...)
}

#' @export
#' @rdname as_learner
as_learners.Learner = function(x, clone = FALSE, ...) { # nolint
  list(if (clone) x$clone(deep = TRUE) else x)
}

#' @title Convert to a Learner
#'
#' @description
#' Convert object to a [Learner] or a list of [Learner].
#'
#' @inheritParams as_task
#'
#' @return [Learner].
#' @export
as_learner = function(x, ...) { # nolint
  UseMethod("as_learner")
}

#' @export
#' @param discard_state (`logical(1)`)
#'   Whether to discard the state.
#' @rdname as_learner
as_learner.Learner = function(x, clone = FALSE, discard_state = FALSE, ...) { # nolint
  if (isTRUE(clone) && isTRUE(discard_state)) {
    clone_without(x, "state")
  } else if (isTRUE(clone)) {
    x$clone(deep = TRUE)
  } else if (isTRUE(discard_state)) {
    x$state = NULL
    x
  } else {
    x
  }
}

#' @export
#' @rdname as_learner
as_learners = function(x, ...) { # nolint
  UseMethod("as_learners")
}

#' @export
#' @rdname as_learner
as_learners.default = function(x, ...) { # nolint
  list(as_learner(x, ...))
}

#' @export
#' @rdname as_learner
as_learners.list = function(x, ...) { # nolint
  lapply(x, as_learner, ...)
}

#' @title Extract Hyperparameter Default Values
#'
#' @description
#' Extract hyperparameter default values.
#'
#' @param x ([Learner])\cr
#'   Learner to extract hyperparameter default values from.
#' @param search_search ([paradox::ParamSet])\cr
#'   Learner hyperparameter values are subsetted to hyperparameters in search space.
#' @param task ([Task])\cr
#'   Task to train the learner on.
#' @param ... (any)\cr
#'   Additional arguments.
#'
#' @return `list()`.
#' @export
default_values = function(x, search_space, task, ...) { # nolint
  UseMethod("default_values")
}

#' @export
#' @rdname default_values
default_values.Learner = function(x, search_space, task, ...) { # nolint
  x$param_set$default[search_space$ids()]
}

#' @export
#' @rdname default_values
default_values.LearnerClassifRpart = function(x, search_space, task, ...) { # nolint
  special_defaults = list(
    minbucket = 20 / 3
  )
  defaults = insert_named(x$param_set$default, special_defaults)
  defaults[search_space$ids()]
}

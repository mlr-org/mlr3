#' @export
default_values.Learner = function(x, search_space, task, ...) { # nolint
  default_values(x$param_set)[search_space$ids()]
}

#' @export
default_values.LearnerClassifRpart = function(x, search_space, task, ...) { # nolint
  special_defaults = list(
    minbucket = 20 / 3
  )
  defaults = insert_named(default_values(x$param_set), special_defaults)
  defaults[search_space$ids()]
}

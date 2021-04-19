#' @title Set the Number of Threads
#'
#' @description
#' Control the parallelism via threading while calling external packages from \CRANpkg{mlr3}.
#'
#' For example, the random forest implementation in package \CRANpkg{ranger} (connected
#' via \CRANpkg{mlr3learners}) supports threading via OpenMP.
#' The number of threads to use can be set via hyperparameter `num.threads`, and
#' defaults to 1. By calling `set_threads(x, 4)` with `x` being a ranger learner, the
#' hyperparameter is changed so that 4 cores are used.
#'
#' If the object `x` does not support threading, `x` is returned as-is.
#' If applied to a list, recurses through all list elements.
#'
#' Note that threading is incompatible with other parallelization techniques such as forking
#' via the [future::plan] [future::multicore]. For this reason all learners connected to \CRANpkg{mlr3}
#' have threading disabled in their defaults.
#'
#' @param x (`any`)\cr
#'   Object to set threads for, e.g. a [Learner].
#'   This object is modified in-place.
#' @param n (`integer(1)`)\cr
#'   Number of threads to use. Defaults to [parallelly::availableCores()].
#'
#' @return Same object as input `x` (changed in-place),
#'   with possibly updated parameter values.
#' @export
set_threads = function(x, n = availableCores()) {
  UseMethod("set_threads")
}

#' @rdname set_threads
#' @export
set_threads.default = function(x, n = availableCores()) { # nolint
  x
}

#' @rdname set_threads
#' @export
set_threads.R6 = function(x, n = availableCores()) { # nolint
  if (exists("param_set", envir = x)) {
    id = x$param_set$ids(tags = "threads")
    if (length(id)) {
      x$param_set$values = insert_named(x$param_set$values, named_list(id, n))
    }
  }
  x
}

#' @rdname set_threads
#' @export
set_threads.list = function(x, n = availableCores()) { # nolint
  lapply(x, set_threads, n = n)
}

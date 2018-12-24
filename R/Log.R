#' @title Learner Output Log
#'
#' @name Log
#' @format [R6Class] object.
#' @description
#' Object which stores the text output of the `train` or `predict` step of an [Experiment].
#'
#' @section Usage:
#' ```
#' # Construction
#' l = Log$new(log = NULL)
#'
#' # Members
#' l$errors
#' l$log
#' l$warnings
#'
#' # Methods
#' l$format()
#' l$has_condition(cl)
#' l$print()
#' ```
#'
#' @section Arguments:
#' * `log` ([data.table()]):
#'   [data.table()] with columns `class` (`character()`) and `message` (`character()`).
#'   Class can be one of "output", "warning", or "error".
#' * `cl` (`character(1)`):
#'   Class of a condition.
#'
#' @section Details:
#' * `$errors` (`character()`) returns all lines which are errors.
#' * `$warnings` (`character()`) returns all lines which are warnings.
#' * `$has_condition(cl)` returns `TRUE` if at least on message of class `cl` is
#'   logged. Possible conditions are "output", "message", "warning", and "error".
#' * `$new(log)` parses the object returned by [evaluate::evaluate] and creates
#'   a new [Log].
#' * `format()` and `print()` are for formatting and printing via [format] or
#'   [print], respectively.
#'
#' @examples
#' # Create a simple experiment and extract the train log:
#' task = mlr_tasks$get("sonar")
#' learner = mlr_learners$get("classif.debug")
#' learner$param_vals = list(message_train = TRUE, error_train = TRUE)
#' learner$fallback = mlr_learners$get("classif.featureless")
#' e = Experiment$new(task, learner)
#' e$train(ctrl = list(encapsulate_train = "evaluate"))
#' l = e$logs$train
#'
#' l$has_condition("error")
#' print(l)
NULL

#' @export
Log = R6Class("Log", cloneable = FALSE,
  public = list(
    log = NULL,
    initialize = function(log) {
      self$log = assert_data_table(log, ncol = 2L)
    },

    format = function() {
      sprintf("<%s>", class(self)[1L])
    },

    print = function() {
      n = nrow(self$log)
      if (n == 0L) {
        catf("Empty <Log>")
      } else {
        catf("%s with %i message%s:", format(self), n, if (n == 1L) "" else "s")
        catf(strwrap(sprintf("%i: [%s] %s", seq_len(n), self$log$class, self$log$msg), exdent = nchar(n) + 2L))
      }
    },

    has_condition = function(cl) {
      assert_choice(cl, mlr_reflections$log_classes)
      nrow(self$log) && self$log[list(cl), .N, on = "class", nomatch = 0L] > 0L
    }
  ),

  active = list(
    warnings = function() {
      self$log[list("warning"), "msg", on = "class", nomatch = 0L, with = FALSE][[1L]]
    },

    errors = function() {
      self$log[list("error"), "msg", on = "class", nomatch = 0L, with = FALSE][[1L]]
    }
  )
)

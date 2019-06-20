#' @title Learner Output Log
#'
#' @usage NULL
#' @format [R6::R6Class] object.
#' @include mlr_reflections.R
#'
#' @description
#' Object which stores the text output of the `train` or `predict` step of an [Experiment].
#'
#' @section Construction:
#' ```
#' l = Log$new(log = NULL)
#' ```
#'
#' * `log` :: [data.table::data.table()]\cr
#'   Table with at least the columns "class" (allowed values are "output", "warning", and "error")
#'   and "msg" (`character()`).
#'   A third column "context" is optional, and currently only used for printing.
#'
#' @section Fields:
#' * `warnings` :: `character(1)`\cr
#'   Vector of all messages of class "warning".
#' * `errors` :: `character(1)`\cr
#'   Vector of all messages of class "error".
#'
#' @section Methods:
#' * `has_condition(cl)`\cr
#'   `character(1)` -> `logical(1)`\cr
#'   Returns `TRUE` if at least one message with condition `cl` has been recorded.
#'
#' @export
#' @examples
#' # Create a simple experiment and extract the train log:
#' task = mlr_tasks$get("sonar")
#' learner = mlr_learners$get("classif.debug",
#'   param_vals = list(message_train = TRUE, error_train = TRUE))
#' e = Experiment$new(task, learner)
#' e$train(ctrl = list(encapsulate_train = "evaluate"))
#' l = e$log()
#'
#' print(l)
#' l$has_condition("error")
Log = R6Class("Log", cloneable = FALSE,
  public = list(
    log = NULL,
    initialize = function(log = NULL) {
      if (is.null(log)) {
        log = data.table(class = character(), msg = character())
      } else {
        assert_data_table(log)
        assert_names(names(log), must.include = c("class", "msg"))
      }
      self$log = log
    },

    format = function() {
      sprintf("<%s>", class(self)[1L])
    },

    print = function() {
      log_print(self)
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

log_print = function(self) {
  n = nrow(self$log)
  if (n == 0L) {
    catf("Empty <Log>")
  } else {
    trans = c(output = "OUT", warning = "WRN", error = "ERR")
    catf("%s with %i message%s:", format(self), n, if (n == 1L) "" else "s")

    if (is.null(self$log$context)) {
      catf(strwrap(sprintf("[%s]: %s", trans[self$log$class], self$log$msg), exdent = nchar(n) + 2L))
    } else {
      catf(strwrap(sprintf("[%s] %s: %s", trans[self$log$class], self$log$context, self$log$msg), exdent = nchar(n) + 2L))
    }
  }
}

#' @title Learner Output Log
#'
#' @usage NULL
#' @format [R6::R6Class] object.
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
#'   Table with columns "class" (allowed values are "output", "warning", and "error")
#'   and "message".
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
#' learner$fallback = mlr_learners$get("classif.featureless")
#' e = Experiment$new(task, learner)
#' e$train(ctrl = list(encapsulate_train = "evaluate"))
#' l = e$logs$train
#'
#' l$has_condition("error")
#' print(l)
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
    },

    append = function(cl, msg) {
      self$log = rbind(self$log, data.table(
          class = assert_choice(cl, mlr_reflections$log_classes),
          msg = assert_string(msg))
      )
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

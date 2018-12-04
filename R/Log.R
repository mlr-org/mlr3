#' @title Learner Output Log
#'
#' @description
#' Object which stores the output of the `train` or `predict` step of an [Experiment].
#'
#' @section Usage:
#' ```
#' # Construction
#' l = Log$new(log = NULL)
#' #
#' l$has_condition(cl)
#' l$format()
#' l$print()
#' ```
#'
#' @section Arguments:
#' * `log`:
#'   Object as returned by [evaluate::evaluate()].
#' * `cl` (`character(1)`):
#'   Class of a condition. One of "output", "message", "warning", or "error".
#'
#' @section Details:
#' * `$new(log)` parses the object returned by [evaluate::evaluate()] and creates a new [Log].
#'
#'
#' * `$has_condition(cl)` returns `TRUE` if at least on message of class `cl` is logged.
#'   Possible conditions are "output", "message", "warning", and "error".
#'
#' * `format()` and `print()` are for formatting and printing via [format()] or [print()], respectively.
#'
#' @name Log
#' @examples
#' # Create a simple experiment and extract the train log:
#' task = mlr_tasks$get("sonar")
#' learner = mlr_learners$get("classif.crashtest")
#' learner$fallback = mlr_learners$get("classif.featureless")
#' e = Experiment$new(task, learner)
#' e$train()
#' log = e$logs$train
#'
#' log$has_condition("error")
#' log$print()
NULL

#' @export
Log = R6Class("Log", cloneable = FALSE,
  public = list(
    messages = NULL,
    initialize = function(log = NULL) {
      if (length(log) <= 1L) {
        self$messages = data.table(msg = character(0L), class = factor(character(0L), levels = mlr_reflections$log_classes))
      } else {
        self$messages = parse_evaluate(log)
      }
    },

    format = function() {
      sprintf("[%s] %s", self$messages$class, self$messages$msg)
    },

    print = function() {
      n = nrow(self$messages)
      catf("<Log> with %i message%s:", n, if (n == 1L) "" else "s")
      if (n > 0L)
        catf(strwrap(paste0(seq_len(n), ": ", format(self)), exdent = nchar(n) + 2L))
    },

    has_condition = function(cl) {
      assert_choice(cl, mlr_reflections$log_classes)
      nrow(self$messages) && self$messages[list(cl), .N, on = "class", nomatch = 0L] > 0L
    }
  )
)

parse_evaluate = function(log) {
  translate_class = function(x) {
    if (is.character(x))
      return("output")
    if (inherits(x, "message"))
      return("message")
    if (inherits(x, "warning"))
      return("warning")
    if (inherits(x, "error"))
      return("error")
    stop("Unknown log class while parsing log")
  }

  log = log[-1L] # remove $src
  data.table(
    msg = map_chr(log, function(x) trimws(if (is.character(x)) x else x$message)),
    class = factor(map_chr(log, translate_class), levels = mlr_reflections$log_classes)
  )
}

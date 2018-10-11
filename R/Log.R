Log = R6Class("Log", cloneable = FALSE,
  public = list(
    messages = NULL,
    initialize = function(log = NULL) {
      if (length(log) <= 1L) {
        self$messages = data.table(msg = character(0L), class = factor(character(0L), levels = reflections$log_classes))
      } else {
        self$messages = parse_evaluate(log)
      }
    },

    format = function() {
      sprintf("[%s] %s", self$messages$class, self$messages$msg)
    },

    print = function() {
      n = nrow(self$messages)
      catf("<Log> with %i messages", n)
      if (n > 0L)
        catf(format(self))
    },

    is_empty = function() {
      nrow(self$messages) == 0L
    },

    has_conditions = function(classes) {
      assert_character(classes, min.len = 1L, any.missing = FALSE)
      nrow(self$messages) == 0L || self$messages[list(classes), .N, on = "class", nomatch = 0L] > 0L
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
    msg = vcapply(log, function(x) trimws(if (is.character(x)) x else x$message)),
    class = factor(vcapply(log, translate_class), levels = reflections$log_classes)
  )
}

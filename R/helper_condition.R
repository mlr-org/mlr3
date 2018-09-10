# formating cat()
catf = function (..., con = "") {
  cat(paste0(sprintf(...), collapse = "\n"), "\n", sep = "", file = con)
}

# formating waring()
warningf = function (...) {
  warning(simpleWarning(sprintf(...), call = NULL))
}

# formating stop()
stopf = function (...) {
  stop(simpleError(sprintf(...), call = NULL))
}

info = function(msg, ...) {
  if (isTRUE(getOption("mlr3.verbose")))
    cat(sprintf(msg, ...), "\n")
}

debug = function(msg, ...) {
  if (isTRUE(getOption("mlr3.debug")))
    cat(sprintf(msg, ...), "\n")
}

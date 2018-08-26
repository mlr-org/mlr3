# formating cat()
catf = function (..., con = "") {
  cat(stri_flatten(sprintf(...), "\n"), "\n", sep = "", file = con)
}

# formating message()
messagef = function (..., con = "") {
  if (isTRUE(getOption("mlr3.verbose")))
    message(sprintf(...))
}

# formating waring()
warningf = function (...) {
  warning(simpleWarning(sprintf(...), call = NULL))
}

# formating stop()
stopf = function (...) {
  stop(simpleError(sprintf(...), call = NULL))
}

info = function(msg, ...) {
  if (isTRUE(getOption("mlr3.verbose")))
    messagef(msg, ...)
}

debug = function(msg, ...) {
  if (isTRUE(getOption("mlr3.debug", FALSE)))
    message("[debug] ", sprintf(msg, ...))
}

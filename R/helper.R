require_namespaces = function(pkgs, msg = "The following packages are missing: %s") {
  ok = vlapply(unique(pkgs), requireNamespace, quietly = TRUE)
  if (!all(ok))
    stopf(msg, stri_flatten(pkgs[!ok], ","))
}

ids = function(x) {
  vcapply(x, "[[", "id")
}

# do.call with evaluate
# this might get superseded by futures with output logging
ecall = function(fun, pars) {
  result = NULL
  log = evaluate::evaluate(
    "result <- do.call(fun, pars)",
    new_device = FALSE,
    include_timing = FALSE
  )

  if (length(log) == 1L) {
    log = data.table(msg = character(0L), type = character(0L))
  } else {
    log = log[-1L] # remove $src
    msg = vcapply(log, function(x) if (is.character(x)) x else x$message)
    type = vcapply(log, function(x) {
      if (is.character(x))
        return("output")
      if (inherits(x, "message"))
        return("message")
      if (inherits(x, "warning"))
        return("warning")
      if (inherits(x, "error"))
        return("error")
      stop("Unknown type while parsing log")
    })
    log = data.table(msg = msg, type = type)
  }
  log$type = factor(log$type, levels = c("output", "message", "warning", "error"))

  list(
    result = result,
    log = log
  )
}

shuffle = function(x) {
  # a "safe" sample() for n == length(x)
  if (length(x) <= 1L)
    return(x)
  sample(x)
}

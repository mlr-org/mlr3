encapsulate = function(ctrl) {
  opt = ctrl$encapsulate
  if (is.null(opt))
    return(encapsulate_dummy)
  switch(opt,
    "evaluate" = encapsulate_evaluate,
    "callr" = encapsulate_callr,
    stopf("Unkown encapsulation method '%s'", opt)
  )
}

encapsulate_dummy = function(fun, args = list()) {
  now = proc.time()[[3L]]
  result = do.call(fun, args)
  elapsed = proc.time()[[3L]] - now
  log = setDT(list(class = character(), msg = character()))
  list(result = result, log = Log$new(log), elapsed = elapsed)
}


encapsulate_evaluate = function(fun, args = list()) {
  parse_evaluate = function(log) {
    translate_class = function(x) {
      if (inherits(x, "warning"))
        return("warning")
      if (inherits(x, "error"))
        return("error")
      return("output")
    }

    log = log[-1L] # remove $src
    data.table(
      class = map_chr(log, translate_class),
      msg = map_chr(log, function(x) trimws(if (is.character(x)) x else x$message))
    )
  }

  require_namespaces("evaluate")
  now = proc.time()[[3L]]
  result = NULL
  log = evaluate::evaluate(
    "result <- do.call(fun, args)",
    stop_on_error = 1L,
    new_device = FALSE,
    include_timing = FALSE
  )
  elapsed = proc.time()[[3L]] - now
  log = parse_evaluate(log)
  list(
    result = result,
    log = Log$new(log),
    elapsed = elapsed
  )
}

encapsulate_callr = function(fun, args = list()) {
  wrapper = function(fun, args) {
    options(warn = 1L)
    now = proc.time()[[3L]]
    result = withCallingHandlers(
        tryCatch(do.call(fun, args),
          error = function(e) {
            cat("[ERR]", gsub("\r?\n|\r", "<br>", trimws(conditionMessage(e))), "\n")
            NULL
          }
        ),
        warning = function(w) {
          cat("[WRN]", gsub("\r?\n|\r", "<br>", trimws(conditionMessage(w))), "\n")
          invokeRestart("muffleWarning")
        }
    )
    list(result = result, elapsed = proc.time()[[3L]] - now)
  }


  require_namespaces("callr")

  logfile = tempfile()
  now = proc.time()[3L]
  result = try(callr::r(wrapper, list(fun = fun, args = args), stdout = logfile, stderr = logfile), silent = TRUE)
  elapsed = proc.time()[3L] - now

  if (file.exists(logfile)) {
    lines = readLines(logfile, warn = FALSE)
    file.remove(logfile)
  } else {
    lines = character(0L)
  }

  if (inherits(result, "try-error")) {
    status = attr(result, "condition")$status
    lines = c(lines, sprintf("[ERR] callr exited with status %i", status))
    result = NULL
  } else {
    elapsed = result$elapsed
    result = result$result
  }

  if (length(lines)) {
    log = data.table(class = "output", msg = lines)
    parse_line = function(x) gsub("<br>", "\n", substr(x, 7L, nchar(x)))
    log[startsWith(get("msg"), "[WRN] "), c("class", "msg") := list("warning", parse_line(msg))]
    log[startsWith(get("msg"), "[ERR] "), c("class", "msg") := list("error", parse_line(msg))]
  } else {
    log = data.table(class = character(0L), msg = character(0L))
  }

  list(result = result, log = Log$new(log), elapsed = elapsed)
}

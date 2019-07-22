encapsulate = function(method) {
  if (is.na(method))
    return(encapsulate_dummy)
  switch(method,
    "none" = encapsulate_dummy,
    "evaluate" = encapsulate_evaluate,
    "callr" = encapsulate_callr,
    stopf("Unkown encapsulation method '%s'", method)
  )
}

encapsulate_dummy = function(fun, args = list(), pkgs = character(0L), seed = NA_integer_) {
  require_namespaces(pkgs)
  if (!is.na(seed)) {
    seed = get_seed()
    set.seed(seed)
    on.exit(assign(".Random.seed", seed, globalenv()), add = TRUE)
  }
  now = proc.time()[[3L]]
  result = invoke(fun, .args = args)
  elapsed = proc.time()[[3L]] - now
  list(result = result, log = NULL, elapsed = elapsed)
}


encapsulate_evaluate = function(fun, args = list(), pkgs = character(0L), seed = NA_integer_) {

  parse_evaluate = function(log) {
    extract = function(x) {
      if (inherits(x, "message")) {
        return(list(class = "output", msg = trimws(x$message)))
      }
      if (inherits(x, "warning")) {
        return(list(class = "warning", msg = trimws(x$message)))
      }
      if (inherits(x, "error")) {
        return(list(class = "error", msg = trimws(x$message)))
      }
      if (inherits(x, "recordedplot")) {
        return(NULL)
      }
      return(list(class = "output", msg = trimws(x)))
    }

    log = map_dtr(log[-1L], extract)
    if (ncol(log) == 0L) NULL else log
  }

  require_namespaces(c("evaluate", pkgs))
  now = proc.time()[[3L]]
  result = NULL
  if (!is.na(seed)) {
    seed = get_seed()
    set.seed(seed)
    on.exit(assign(".Random.seed", seed, globalenv()), add = TRUE)
  }
  log = evaluate::evaluate(
    "result <- do.call(fun, args)",
    stop_on_error = 1L,
    new_device = FALSE,
    include_timing = FALSE
  )
  elapsed = proc.time()[[3L]] - now
  log = parse_evaluate(log)
  list(result = result, log = log, elapsed = elapsed)
}

encapsulate_callr = function(fun, args = list(), pkgs = character(0L), seed = NA_integer_) {

  wrapper = function(fun, args, pkgs, seed) {
    options(warn = 1L)
    suppressPackageStartupMessages({
      library("mlr3")
      lapply(pkgs, requireNamespace)
    })
    if (!is.na(seed)) {
      set.seed(seed)
    }
    now = proc.time()[[3L]]
    result = withCallingHandlers(
      tryCatch(do.call(fun, args),
        error = function(e) {
          cat("[ERR]", gsub("\r?\n|\r", "<br>", conditionMessage(e)), "\n")
          NULL
        }),
      warning = function(w) {
        cat("[WRN]", gsub("\r?\n|\r", "<br>", conditionMessage(w)), "\n")
        invokeRestart("muffleWarning")
      })
    list(result = result, elapsed = proc.time()[[3L]] - now)
  }

  require_namespaces("callr")

  logfile = tempfile()
  now = proc.time()[3L]
  result = try(callr::r(wrapper, list(fun = fun, args = args, pkgs = pkgs, seed = seed), stdout = logfile, stderr = logfile), silent = TRUE)
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
    msg = NULL
    log = data.table(class = "output", msg = lines)
    parse_line = function(x) trimws(gsub("<br>", "\n", substr(x, 7L, nchar(x)), fixed = TRUE))
    log[startsWith(get("msg"), "[WRN] "), c("class", "msg") := list("warning", parse_line(msg))]
    log[startsWith(get("msg"), "[ERR] "), c("class", "msg") := list("error", parse_line(msg))]
  } else {
    log = NULL
  }

  list(result = result, log = log, elapsed = elapsed)
}

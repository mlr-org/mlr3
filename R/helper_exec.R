allow_partial_matching = list(
  warnPartialMatchArgs = FALSE,
  warnPartialMatchAttr = FALSE,
  warnPartialMatchDollar = FALSE
)


set_encapsulation = function(learners, encapsulate) {
  assert_choice(encapsulate, c(NA_character_, "none", "evaluate", "callr"))

  if (!is.na(encapsulate)) {
    lapply(learners, function(learner) learner$encapsulate = c(train = encapsulate, predict = encapsulate))
    if (encapsulate %in% c("evaluate", "callr")) {
      task_type = unique(map_chr(learners, "task_type"))
      stopifnot(length(task_type) == 1L) # this should not be possible for benchmarks
      fb = get_featureless_learner(task_type)
      if (!is.null(fb)) {
        lapply(learners, function(learner) if (is.null(learner$fallback)) learner$fallback = fb$clone(TRUE))
      }
    }
  }
  learners
}

future_map = function(n, FUN, ..., MoreArgs = list()) {
  if (getOption("mlr3.debug", FALSE)) {
    lg$info("Running experiments sequentially in debug mode with %i iterations", n)
    mapply(FUN, ..., MoreArgs = MoreArgs, SIMPLIFY = FALSE, USE.NAMES = FALSE)
  } else {
    is_sequential = inherits(plan(), "sequential")
    scheduling = if (!is_sequential && isTRUE(getOption("mlr3.exec_random", TRUE))) structure(TRUE, ordering = "random") else TRUE
    chunk_size = getOption("mlr3.exec_chunk_size", 1)
    stdout = if (is_sequential) NA else TRUE

    lg$debug("Running resample() via future with %i iterations", n)
    future.apply::future_mapply(
      FUN, ..., MoreArgs = MoreArgs, SIMPLIFY = FALSE, USE.NAMES = FALSE,
      future.globals = FALSE, future.packages = "mlr3", future.seed = TRUE,
      future.scheduling = scheduling, future.chunk.size = chunk_size, future.stdout = stdout)
  }
}

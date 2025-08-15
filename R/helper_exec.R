allow_partial_matching = list(
  warnPartialMatchArgs = FALSE,
  warnPartialMatchAttr = FALSE,
  warnPartialMatchDollar = FALSE
)

set_encapsulation = function(learners, encapsulate) {
  assert_choice(encapsulate, c(NA_character_, "none", "evaluate", "callr", "try"))

  if (!is.na(encapsulate)) {
    lapply(learners, function(learner) {
      fallback = if (encapsulate != "none") default_fallback(learner)
      if (is.null(fallback)) {
        stopf("Could not find default fallback learner for learner '%s'", learner$id)
      }
      learner$encapsulate(encapsulate, fallback)
    })
  }
  learners
}

future_map = function(n, FUN, ..., MoreArgs = list()) {
  if (getOption("mlr3.debug", FALSE)) {
    lg$info("Running experiments sequentially in debug mode with %i iterations", n)
    mapply(FUN, ..., MoreArgs = MoreArgs, SIMPLIFY = FALSE, USE.NAMES = FALSE)
  } else if (isNamespaceLoaded("mirai") && mirai::daemons_set(.compute = getOption("mlr3.mirai_parallelization", "mlr3_parallelization"))) {
    lg$debug("Running resample() via mirai with %i iterations", n)
    mirai::collect_mirai(mirai::mirai_map(data.table(...), FUN,
      .args = c(MoreArgs, list(is_sequential = FALSE)),
      .compute = getOption("mlr3.mirai_parallelization", "mlr3_parallelization")))
  } else {
    is_sequential = inherits(plan(), "sequential")
    scheduling = if (!is_sequential && isTRUE(getOption("mlr3.exec_random", TRUE))) structure(TRUE, ordering = "random") else TRUE
    chunk_size = getOption("mlr3.exec_chunk_size", 1)
    chunk_bins = getOption("mlr3.exec_chunk_bins")
    if (!is.null(chunk_bins)) {
      chunk_size = ceiling(n / chunk_bins)
    }
    stdout = if (is_sequential) NA else TRUE

    # workaround for sequential plan checking the size of the globals
    # see https://github.com/futureverse/future/issues/197
    if (is_sequential) {
      old_opts = options(future.globals.maxSize = Inf)
      on.exit(options(old_opts), add = TRUE)
    }

    MoreArgs = c(MoreArgs, list(is_sequential = is_sequential))

    lg$debug("Running resample() via future with %i iterations", n)
    future.apply::future_mapply(
      FUN, ..., MoreArgs = MoreArgs, SIMPLIFY = FALSE, USE.NAMES = FALSE,
      future.globals = FALSE, future.packages = mlr_reflections$loaded_packages, future.seed = TRUE,
      future.scheduling = scheduling, future.chunk.size = chunk_size, future.stdout = stdout
    )
  }
}

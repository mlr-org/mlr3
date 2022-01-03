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

future_stdout = function() {
  if (inherits(plan(), "sequential")) {
    NA
  } else {
    TRUE
  }
}

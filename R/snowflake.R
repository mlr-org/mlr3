snowflake_init = function() {
  snowflake = list(
    fact = data.table(
      uhash = character(),
      iteration = integer(),
      state = list(),
      prediction = list(),
      key = "uhash"
    ),

    uhash = data.table(
      uhash = character(),
      task_hash = character(),
      learner_hash = character(),
      resampling_hash = character(),
      key = "uhash"
    ),

    tasks = data.table(
      task_hash = character(),
      task_phash = character(),
      feature_names = list(),
      key = "task_hash"
    ),

    learners = data.table(
      learner_hash = character(),
      learner_phash = character(),
      param_vals = list(),
      key = "learner_hash"
    ),

    resamplings = data.table(
      resampling_hash = character(),
      resampling = list(),
      key = "resampling_hash"
    ),

    task_objs = data.table(
      task_phash = character(),
      task = list(),
      key = "task_phash"
    ),

    learner_objs = data.table(
      learner_phash = character(),
      learner = list(),
      key = "learner_phash"
    )
  )

  class(snowflake) = "snowflake"
  snowflake
}

snowflake_copy = function(snowflake) {
  set_class(lapply(snowflake, copy), "snowflake")
}

snowflake_fill = function(snowflake, data) {
  rbind_if_new = function(x, y, on = key(x)) {
    if (nrow(x) == 0L) {
      return(y)
    }
    new_rows = y[!list(x), on = on, which = TRUE]
    if (length(new_rows)) {
      setkeyv(rbindlist(list(x, y[, names(x), with = FALSE]), use.names = FALSE), on)
    } else {
      x
    }
  }

  fact = data[, c("uhash", "iteration", "state", "prediction"), with = FALSE]
  snowflake$fact = rbind_if_new(snowflake$fact, fact)

  uhash = data[, list(
    task = list(task[[1L]]),
    task_hash = task[[1L]]$hash,
    learner = list(learner[[1L]]),
    learner_hash = learner[[1L]]$hash,
    resampling = list(resampling[[1L]]),
    resampling_hash = resampling[[1L]]$hash
  ), by = "uhash"]
  snowflake$uhash = rbind_if_new(snowflake$uhash, uhash)

  tasks = uhash[, list(
    task = list(task[[1L]]),
    task_phash = task[[1L]]$phash,
    feature_names = list(task[[1L]]$feature_names)
  ), by = "task_hash"]
  snowflake$tasks = rbind_if_new(snowflake$tasks, tasks)

  learners = uhash[, list(
    learner = list(learner[[1L]]),
    learner_phash = learner[[1L]]$phash,
    param_vals = list(learner[[1L]]$param_set$values)
  ), by = "learner_hash"]
  snowflake$learners = rbind_if_new(snowflake$learners, learners)

  resamplings = uhash[, list(
    resampling = list(resampling[[1L]])
  ), by = "resampling_hash"]
  snowflake$resamplings = rbind_if_new(snowflake$resamplings, resamplings)

  task_objs = tasks[, list(
    task = list(task[[1L]])
  ), by = "task_phash"]
  snowflake$task_objs = rbind_if_new(snowflake$task_objs, task_objs)

  learner_objs = learners[, list(
    learner = list(learner[[1L]])
  ), by = "learner_phash"]
  snowflake$learner_objs = rbind_if_new(snowflake$learner_objs, learner_objs)

  return(snowflake)
}

#' @export
as.data.table.snowflake = function(x, ..., reassemble_tasks = TRUE, reassemble_learners = TRUE, convert_predictions = TRUE, predict_sets = "test") {

  tab = x$uhash
  tab = merge(tab, x$tasks, by = "task_hash", sort = FALSE)
  tab = merge(tab, x$task_objs, by = "task_phash", sort = FALSE)
  tab = merge(tab, x$learners, by = "learner_hash", sort = FALSE)
  tab = merge(tab, x$learner_objs, by = "learner_phash", sort = FALSE)
  tab = merge(tab, x$resamplings, by = "resampling_hash", sort = FALSE)
  tab = merge(tab, x$fact, all.y = TRUE, by = "uhash", sort = FALSE)
  setorderv(tab, "iteration")

  if (reassemble_tasks) {
    set(tab, j = "task", value = reassemble_tasks(tab$task, tab$feature_names))
  }

  if (reassemble_learners) {
    set(tab, j = "learner", value = reassemble_learners(tab$learner, tab$state, tab$param_vals))
  }

  if (convert_predictions) {
    set(tab, j = "prediction", value = as_predictions(tab$prediction, predict_sets = predict_sets))
  }

  tab[, c("uhash", "task", "task_hash", "learner", "learner_hash", "resampling", "resampling_hash", "iteration", "prediction"), with = FALSE]
}

snowflake_filter = function(snowflake, uhashes = NULL) {
  # if (!is.null(iterations)) {
  #   snowflake$fact = snowflake$fact[list(iterations), on = "iteration", nomatch = NULL]
  # }

#   if (!is.null(uhashes)) {
#     snowflake$fact = snowflake$fact[list(unique(uhashes)), on = "uhash", nomatch = NULL]
#   }


  new_data = set_class(list(
    fact = snowflake$fact[list(needle), on = "uhash"],
    uhash = snowflake$uhash[list(needle), on = "uhash"],
    tasks = snowflake$tasks[list(uhash$task_hash), on = "task_hash"],
    learners = snowflake$learners[list(uhash$learner_hash), on = "learner_hash"],
    resamplings = snowflake$resamplings[list(uhash$resampling_hash), on = "resampling_hash"],
    task_objs = snowflake$task_objs[list(tasks$task_phash), on = "task_phash"],
    learner_objs = snowflake$learner_objs[list(learners$learner_phash), on = "learner_phash"]
  ), "snowflake")

  rr = ResampleResult$new(data = new_data)
}

snowflake_rr_result = function(snowflake, uhash) {
  needle = uhash

  fact = snowflake$fact[list(needle), on = "uhash"]
  uhash = snowflake$uhash[list(needle), on = "uhash"]
  tasks = snowflake$tasks[list(uhash$task_hash), on = "task_hash"]
  learners = snowflake$learners[list(uhash$learner_hash), on = "learner_hash"]
  resamplings = snowflake$resamplings[list(uhash$resampling_hash), on = "resampling_hash"]
  task_objs = snowflake$task_objs[list(tasks$task_phash), on = "task_phash"]
  learner_objs = snowflake$learner_objs[list(learners$learner_phash), on = "learner_phash"]

  new_data = set_class(list(fact = fact, uhash = uhash, tasks = tasks, learners = learners,
      resamplings = resamplings, task_objs = task_objs, learner_objs = learner_objs), "snowflake")

  ResampleResult$new(data = new_data)
}

rdata_init = function() {
  fact = data.table(
    uhash = character(),
    iteration = integer(),
    learner_state = list(),
    prediction = list(),

    task_hash = character(),
    task_phash = character(),
    learner_hash = character(),
    learner_phash = character(),
    resampling_hash = character(),

    key = c("uhash", "iteration")
  )

  tasks = data.table(
    task_phash = character(),
    task = list(),
    key = "task_phash"
  )

  learners = data.table(
    learner_phash = character(),
    learner = list(),
    key = "learner_phash"
  )

  resamplings = data.table(
    resampling_hash = character(),
    resampling = list(),
    key = "resampling_hash"
  )

  task_components = data.table(
    task_hash = character(),
    task_feature_names = list(),
    key = "task_hash"
  )

  learner_components = data.table(
    learner_hash = character(),
    learner_param_vals = list(),
    key = "learner_hash"
  )

  set_class(list(fact = fact, tasks = tasks, learners = learners, resamplings = resamplings,
    task_components = task_components, learner_components = learner_components), "ResultData")
}

rdata_copy = function(rdata, exclude = character()) {
  nn = setdiff(names(rdata), exclude)
  rdata[nn] = lapply(rdata[nn], copy)
  rdata
}

rdata_from_table = function(tab) {
  assert_names(names(tab),
    permutation.of = c("task", "learner", "state", "resampling", "iteration", "prediction", "uhash"))

  if (nrow(tab) == 0L) {
    return(rd_init())
  }

  # TODO: change character to factor?
  fact = setkeyv(tab[, list(
    uhash = uhash,
    iteration = iteration,
    learner_state = state,
    prediction = prediction,

    task = task,
    learner = learner,
    resampling = resampling,

    task_hash = hashes(task),
    task_phash = phashes(task),
    learner_hash = hashes(learner),
    learner_phash = phashes(learner),
    resampling_hash = hashes(resampling)
  )], c("uhash", "iteration"))

  # reconstruct this for filtering?
  tasks = fact[, list(task = task[1L]), keyby = "task_phash"]
  learners = fact[, list(learner = learner[1L]), keyby = "learner_phash"]
  resamplings = fact[, list(resampling = resampling[1L]), keyby = "resampling_hash"]
  task_components = fact[, list(task_feature_names = list(task[[1L]]$feature_names)), keyby = "task_hash"]
  learner_components = fact[, list(learner_param_vals = list(learner[[1L]]$param_set$values)), keyby = "learner_hash"]

  set(fact, j = "task", value = NULL)
  set(fact, j = "learner", value = NULL)
  set(fact, j = "resampling", value = NULL)

  set_class(list(fact = fact, tasks = tasks, learners = learners, resamplings = resamplings,
    task_components = task_components, learner_components = learner_components), "ResultData")
}

rd_append = function(x, y) {
  assert_class(x, "ResultData")
  assert_class(y, "ResultData")

  rbind_if_new = function(x, y, on = key(x)) {
    if (nrow(x) == 0L) {
      return(y[, names(x), with = FALSE])
    }

    new_rows = y[!list(x), on = on, which = TRUE]
    if (length(new_rows)) {
      setkeyv(rbindlist(list(x, y[new_rows]), use.names = TRUE), on)
    } else {
      x
    }
  }

  for (nn in names(x)) {
    x[[nn]] = rbind_if_new(x[[nn]], y[[nn]])
  }

  return(x)
}

#' @export
as.data.table.ResultData = function(x, ..., hashes = TRUE, reassemble_tasks = TRUE, reassemble_learners = TRUE, convert_predictions = TRUE, predict_sets = "test") { # nolint
  tab = x$fact
  tab = merge(tab, x$tasks, by = "task_phash", sort = FALSE)
  tab = merge(tab, x$learners, by = "learner_phash", sort = FALSE)
  tab = merge(tab, x$resamplings, by = "resampling_hash", sort = FALSE)
  tab = merge(tab, x$task_components, by = "task_hash", sort = FALSE)
  tab = merge(tab, x$learner_components, by = "learner_hash", sort = FALSE)

  if (nrow(tab)) {
    # FIXME: do this before the merge
    if (reassemble_tasks) {
      tab[, task := list(reassemble_tasks(task[1L], task_feature_names[1L])), by = "task_phash"]
    }

    if (reassemble_learners) {
      set(tab, j = "learner", value = reassemble_learners(tab$learner, tab$state, tab$param_vals))
    }

    if (convert_predictions) {
      set(tab, j = "prediction", value = as_predictions(tab$prediction, predict_sets = predict_sets))
    }

  }

  cns = c("uhash", "task", "task_hash", "learner", "learner_hash", "resampling",
      "resampling_hash", "iteration", "prediction")
  if (!hashes) {
    cns = setdiff(cns, c("task_hash", "learner_hash", "resampling_hash"))
  }
  tab[, cns, with = FALSE]
}


rdata_sweep = function(rdata) {
  fact = rdata$fact
  for (nn in setdiff(names(rdata), "fact")) {
    tab = rdata[[nn]]
    keycol = key(tab)
    y = unique(fact[, keycol, with = FALSE], by = keycol)
    rdata[[nn]] = merge(tab, y, by = keycol)
  }

  return(rdata)
}


rdata_subset = function(rdata, needle, temporary = TRUE) {
  if (!temporary) {
    rdata = rdata_copy(rdata, exclude = "fact")
  }

  fact = rdata$fact[list(needle), on = "uhash"]

  if (!temporary) {
    rdata = rdata_sweep(rdata)
  }

  return(rdata)
}

rdata_get_tasks = function(rdata, reassemble = TRUE) {
  tab = unique(rdata$fact[, c("task_hash", "task_phash"), with = FALSE], by = "task_hash")
  tab = merge(tab, rdata$tasks, by = "task_phash", sort = FALSE)

  if (reassemble) {
    feature_names = rdata$task_components[list(tab$task_hash), get("task_feature_names"), on = "task_hash", nomatch = NULL]
    set(tab, j = "task", value = reassemble_tasks(tab$task, feature_names))
  }

  return(tab)
}

rdata_get_learners = function(rdata, reassemble = TRUE, states = FALSE) {
  if (states) {
    tab = rdata$fact[, c("learner_hash", "learner_phash", "learner_state"), with = FALSE]
    tab = merge(tab, rdata$learners, by = "learner_phash", sort = FALSE)
    tab = merge(tab, rdata$learner_components, by = "learner_hash", sort = FALSE)
    set(tab, j = "learner", value = reassemble_learners(tab$learner, tab$learner_param_vals, tab$learner_state))
    remove_named(tab, c("learner_state", "learner_param_vals"))
  } else {
    tab = unique(rdata$fact[, c("learner_hash", "learner_phash"), with = FALSE], by = "learner_hash")
    tab = merge(tab, rdata$learners, by = "learner_phash", sort = FALSE)

    if (reassemble) {
      param_vals = rdata$learner_components[list(tab$learner_hash), get("learner_param_vals"), on = "learner_hash", nomatch = NULL]
      set(tab, j = "learner", value = reassemble_learners(tab$learner, param_vals))
    }
  }

  return(tab)
}

rdata_get_resamplings = function(rdata) {
  tab = unique(rdata$fact[, "resampling_hash", with = FALSE], by = "resampling_hash")
  tab = merge(tab, rdata$resamplings, by = "resampling_hash", sort = FALSE)
  return(tab)
}

##' @export
#print.snowflake = function(x, ...) {
#  catf("List of relational data tables")
#  catf("%i iterations, ") ...
#}

#' @title Manually construct an object of type ResultData
#'
#' @description
#' This function allows to manually construct a [ResampleResult] or [BenchmarkResult] by combining
#' the individual components to an object of class `ResultData`, mlr3's internal container object.
#' A [ResampleResult] or [BenchmarkResult] can then be initialized with the returned object.
#' Note that [ResampleResult]s can be converted to a [BenchmarkResult] with [as_benchmark_result()]
#' and multiple [BenchmarkResult]s can be combined to a larger [BenchmarkResult].
#'
#' @param task ([Task]).
#' @param learners (list of trained [Learner]s).
#' @param resampling ([Resampling]).
#' @param iterations (`integer()`).
#' @param predictions (list of [Prediction]s).
#' @param learner_states (`list()`)\cr
#'   Learner states. If not provided, the states of `learners` are automatically extracted.
#'
#' @return `ResultData` object which can be passed to the constructor of [ResampleResult].
#' @export
#' @examples
#' task = tsk("iris")
#' learner = lrn("classif.rpart")
#' resampling = rsmp("cv", folds = 2)$instantiate(task)
#' iterations = seq_len(resampling$iters)
#'
#' # manually train two learners.
#' # store learners and predictions
#' learners = list()
#' predictions = list()
#' for (i in iterations) {
#'   l = learner$clone(deep = TRUE)
#'   learners[[i]] = l$train(task, row_ids = resampling$train_set(i))
#'   predictions[[i]] = l$predict(task, row_ids = resampling$test_set(i))
#' }
#'
#' rdata = as_result_data(task, learners, resampling, iterations, predictions)
#' ResampleResult$new(rdata)
as_result_data = function(task, learners, resampling, iterations, predictions, learner_states = NULL) {
  assert_integer(iterations, any.missing = FALSE, lower = 1L, upper = resampling$iters, unique = TRUE)
  n = length(iterations)

  assert_task(task)
  assert_learners(learners, task = task)
  assert_resampling(resampling, instantiated = TRUE)
  predictions = lapply(predictions, as_prediction_data)
  uhash = UUIDgenerate()

  if (is.null(learner_states)) {
    learner_states = map(learners, "state")
  }

  rdata_from_table(data.table(
    task = list(task),
    learner = learners,
    learner_state = learner_states,
    resampling = list(resampling),
    iteration = iterations,
    prediction = predictions,
    uhash = uhash
  ))
}

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
    permutation.of = c("task", "learner", "learner_state", "resampling", "iteration", "prediction", "uhash"))

  if (nrow(tab) == 0L) {
    return(rdata_init())
  }

  fact = tab[, c("uhash", "iteration", "learner_state", "prediction", "task", "learner", "resampling"), with = FALSE]
  set(fact, j = "task_hash", value = hashes(fact$task))
  set(fact, j = "task_phash", value = phashes(fact$task))
  set(fact, j = "learner_hash", value = hashes(fact$learner))
  set(fact, j = "learner_phash", value = phashes(fact$learner))
  set(fact, j = "resampling_hash", value = hashes(fact$resampling))
  setkeyv(fact, c("uhash", "iteration"))

  tasks = fact[, list(task = .SD$task[1L]), keyby = "task_phash"]
  learners = fact[, list(learner = list(.SD$learner[[1L]]$clone(deep = TRUE)$reset())), keyby = "learner_phash"]
  resamplings = fact[, list(resampling = .SD$resampling[1L]), keyby = "resampling_hash"]
  task_components = fact[, list(task_feature_names = list(.SD$task[[1L]]$feature_names)), keyby = "task_hash"]
  learner_components = fact[, list(learner_param_vals = list(.SD$learner[[1L]]$param_set$values)), keyby = "learner_hash"]

  set(fact, j = "task", value = NULL)
  set(fact, j = "learner", value = NULL)
  set(fact, j = "resampling", value = NULL)

  set_class(list(fact = fact, tasks = tasks, learners = learners, resamplings = resamplings,
    task_components = task_components, learner_components = learner_components), "ResultData")
}

rdata_append = function(x, y) {
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
    # FIXME: do this before the merge?
    if (reassemble_tasks) {
      tab[, "task" := list(reassemble_tasks(.SD$task[1L], .SD$task_feature_names[1L])),
        by = "task_phash", .SDcols = c("task", "task_feature_names")]
    }

    if (reassemble_learners) {
      set(tab, j = "learner", value = reassemble_learners(tab$learner, tab$learner_state, tab$learner_param_vals))
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

rdata_subset = function(fact, rdata) {
  assert_data_table(fact)
  rdata$fact = copy(fact)
  rdata
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
    set(tab, j = "learner", value = reassemble_learners(tab$learner, param_vals = tab$learner_param_vals, states = tab$learner_state))
    remove_named(tab, c("learner_state", "learner_param_vals"))
  } else {
    tab = unique(rdata$fact[, c("learner_hash", "learner_phash"), with = FALSE], by = "learner_hash")
    tab = merge(tab, rdata$learners, by = "learner_phash", sort = FALSE)

    if (reassemble) {
      param_vals = rdata$learner_components[list(tab$learner_hash), get("learner_param_vals"), on = "learner_hash", nomatch = NULL]
      set(tab, j = "learner", value = reassemble_learners(tab$learner, param_vals = param_vals))
    }
  }

  return(tab)
}

rdata_get_resamplings = function(rdata) {
  tab = unique(rdata$fact[, "resampling_hash", with = FALSE], by = "resampling_hash")
  tab = merge(tab, rdata$resamplings, by = "resampling_hash", sort = FALSE)
  return(tab)
}

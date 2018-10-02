library(checkmate)
library(testthat)

if (getOption("mlr3.debug", FALSE)) {
  options(
    warnPartialMatchAttr = TRUE,
    warnPartialMatchDollar = TRUE
  )
}

`[[.R6` = function(x, i, ...) {
  if (i %in% names(x))
    return(get(i, envir = x))
  stop("R6 class ", paste0(class(x), collapse = "/") ," does not have slot '", i, "'!")
}

`$.R6` = function(x, name) {
  if (name %in% names(x))
    return(get(name, envir = x))
  stop("R6 class ", paste0(class(x), collapse = "/") ," does not have slot '", name, "'!")
}

private = function(x) {
  if (!R6::is.R6(x))
    stop("Expected R6 class")
  x$.__enclos_env__[["private"]]
}

with_plan = function(plan, expr) {
  oplan = future::plan()
  on.exit(future::plan(oplan), add = TRUE)
  future::plan(plan)
  force(expr)
}

expect_same_address = function(x, y) {
  testthat::expect_identical(data.table::address(x), data.table::address(y))
}

expect_different_address = function(x, y) {
  testthat::expect_false(identical(data.table::address(x), data.table::address(y)))
}

expect_hash = function(x) {
  expect_string(x, pattern = "^[0-9a-z]{16}$")
}

expect_dictionary = function(d, contains = NA_character_, min.items = 0L) {
  expect_r6(d, "Dictionary")
  expect_environment(d$items)
  expect_string(d$contains)
  expect_character(d$ids, any.missing = FALSE, min.len = min.items, min.chars = 1L)
  if (!is.na(contains)) {
    objs = d$mget(d$ids)
    expect_list(objs, types = contains, names = "unique")
  }
}

expect_backend = function(b) {
  expect_r6(b, cloneable = FALSE, public = c("nrow", "ncol", "colnames", "rownames", "head", "data"))
  n = expect_count(b$nrow)
  p = expect_count(b$ncol)
  cn = expect_atomic_vector(b$colnames, any.missing = FALSE, len = p, unique = TRUE)
  rn = expect_atomic_vector(b$rownames, any.missing = FALSE, len = n, unique = TRUE)

  x = b$data(rows = rn, cols = cn[1L])
  expect_data_table(x, ncol = 1L, nrow = n)
  x = x[[cn[1L]]]
  expect_atomic_vector(x, len = n)

  # extra cols are ignored
  x = b$data(rows = rn[1L], cols = c(cn[1L], "_not_existing_"))
  expect_data_table(x, nrow = 1L, ncol = 1L)

  # zero cols matching
  x = b$data(rows = rn[1L], cols = "_not_existing_")
  expect_data_table(x, nrow = 0L, ncol = 0L)

  # extra rows are ignored
  query_rows = c(rn[1L], if (is.integer(rn)) -1L else "_not_existing_")
  x = b$data(query_rows, cols = cn[1L])
  expect_data_table(x, nrow = 1L, ncol = 1L)

  # zero rows matching
  query_rows = if (is.integer(rn)) -1L else "_not_existing_"
  x = b$data(rows = query_rows, cols = cn[1L])
  expect_data_table(x, nrow = 0L, ncol = 1L)

  # rows are duplicated
  x = b$data(rows = rep(rn[1L], 2L), cols = b$colnames)
  expect_data_table(x, nrow = 2L, ncol = p)

  # rows are returned in the right order
  i = sample(rn, min(n, 10L))
  x = b$data(rows = i, cols = b$primary_key)
  expect_equal(i, x[[1L]])

  # duplicated cols raise exception
  expect_error(b$data(rows = rn[1L], cols = rep(cn[1L], 2L)), "uniquely")

  expect_data_table(b$head(3), nrow = 3, ncol = p)

  expect_atomic_vector(distinct(b$data(rows = rn, cols = b$primary_key)[[1L]]), len = n)
}

expect_task = function(task) {
  expect_r6(task, "Task", cloneable = TRUE, public = c("id", "backend", "task_type", "row_info", "col_info", "order", "head", "row_ids", "feature_names", "target_names", "formula", "nrow", "ncol", "feature_types"))
  expect_string(task$id, min.chars = 1L)
  expect_count(task$nrow)
  expect_count(task$ncol)
  expect_data_table(task$data())
  expect_data_table(task$head(1), nrow = 1L)

  cols = c("id", "role", "type", "levels")
  expect_data_table(task$col_info, key = "id", ncol = length(cols))
  expect_names(names(task$col_info), permutation.of = cols)
  expect_character(task$col_info$id, any.missing = FALSE, unique = TRUE)
  expect_subset(task$col_info$role, capabilities$task_col_roles)
  expect_subset(task$col_info$type, capabilities$task_feature_types)
  expect_list(task$col_info$levels)

  cols = c("id", "role")
  expect_data_table(task$row_info, key = "id", ncol = length(cols))
  expect_names(names(task$row_info), permutation.of = cols)
  expect_atomic_vector(task$row_info$id, any.missing = FALSE, unique = TRUE)
  expect_subset(task$row_info$role, capabilities$task_row_roles)

  types = task$feature_types
  expect_data_table(types, ncol = 2, nrow = length(task$feature_names))
  expect_set_equal(types$id, task$feature_names)
  expect_subset(types$type, capabilities$task_feature_types)

  properties = task$properties
  expect_subset(properties, capabilities$task_properties[[task$task_type]])

  expect_character(task$order, any.missing = FALSE)
  expect_names(task$order, subset.of = c(task$feature_names, task$target_names))

  expect_hash(task$hash)
}

expect_task_supervised = function(task) {
  expect_r6(task, "TaskSupervised", cloneable = TRUE)
  expect_choice(task$target_names, task$col_info$id)

  expect_class(task$formula, "formula")
  expect_null(environment(task$formula))
  tf = terms(task$formula)
  expect_set_equal(labels(tf), task$feature_names) # rhs
  expect_set_equal(setdiff(all.vars(tf), labels(tf)), task$target_names) # lhs
  expect_subset(task$feature_names, colnames(task$head()))
  expect_hash(task$hash)
}

expect_task_classif = function(task) {
  expect_r6(task, "TaskClassif")
  y = task$truth()
  expect_data_table(y, ncol = 1)
  expect_data_table(y, ncol = 1)
  y = y[[1]]
  expect_true(is.character(y) || is.factor(y))

  expect_int(task$class_n, lower = 2L)
  expect_equal(task$class_n, length(unique(y)))
  expect_character(task$class_names, any.missing = FALSE)
  expect_subset(task$class_names, as.character(y))
  if (task$class_n > 2L)
    expect_identical(task$positive, NA_character_)
  else
    expect_choice(task$positive, task$class_names)
  expect_hash(task$hash)
}

expect_task_regr = function(task) {
  expect_r6(task, "TaskRegr")
  y = task$truth()
  expect_data_table(y, ncol = 1)
  y = y[[1]]
  expect_numeric(y, any.missing = FALSE)
  expect_hash(task$hash)
}

expect_learner = function(lrn, task = NULL) {
  expect_r6(lrn, "Learner", cloneable = TRUE)
  expect_output(print(lrn))

  expect_choice(lrn$task_type, capabilities$task_types)
  expect_character(lrn$packages, any.missing = FALSE, min.chars = 1L, unique = TRUE)
  expect_class(lrn$par_set, "ParamSet")
  expect_character(lrn$properties, any.missing = FALSE, min.chars = 1L, unique = TRUE)
  expect_function(lrn$train, args = "task", ordered = TRUE)
  expect_function(lrn$predict, args = c("model", "task"), ordered = TRUE)
  expect_hash(lrn$hash)

  if (!is.null(task)) {
    assert_class(task, "Task")
    expect_subset(lrn$properties, capabilities$learner_properties[[task$task_type]])
    expect_identical(lrn$task_type, task$task_type)
  }
}

expect_resampling = function(r, task = NULL) {
  expect_r6(r, "Resampling")
  expect_string(r$id, min.chars = 1L)

  instance = r$instance
  if (is.null(instance)) {
    expect_false(r$is_instantiated)
    expect_error(r$train_set(1L), "instantiated")
    expect_error(r$test_set(1L), "instantiated")
    expect_identical(r$hash, NA_character_)
    if (r$id != "custom")
      expect_count(r$iters, positive = TRUE)
  } else {
    expect_true(r$is_instantiated)
    expect_hash(r$hash)
    if (!is.null(task))
      ids = task$row_ids()
    expect_count(r$iters, positive = TRUE)

    for (i in seq_len(r$iters)) {
      train = r$train_set(1L)
      test = r$test_set(1L)
      expect_atomic_vector(train, any.missing = FALSE)
      expect_atomic_vector(test, any.missing = FALSE)
      expect_length(intersect(train, test), 0L)
      if (!is.null(task)) {
        expect_subset(train, ids)
        expect_subset(train, ids)
      }
    }
  }
  expect_list(r$par_vals, names = "unique")
  expect_true(qtestr(r$par_vals, "V1"))
}

expect_measure = function(m) {
  expect_r6(m, "Measure", public = c("aggregate", "calculate", "id", "minimize", "packages", "range", "task_type"))

  expect_string(m$id, min.chars = 1L)
  expect_subset(m$task_type, c(NA_character_, capabilities$task_types), empty.ok = FALSE)
  expect_numeric(m$range, len = 2, any.missing = FALSE)
  expect_lt(m$range[1], m$range[2])
  expect_flag(m$minimize)
  expect_character(m$packages, min.chars = 1L, any.missing = FALSE, unique = TRUE)
  expect_function(m$calculate, args = "experiment")
  expect_function(m$aggregate, args = "rr")
}

expect_experiment = function(e) {
  expect_r6(e, "Experiment")
  state = e$state
  expect_factor(state, ordered = TRUE)
  expect_subset(as.character(state), levels(reflections$experiment_slots$state))
  expect_list(e$data, len = nrow(reflections$experiment_slots))
  expect_names(names(e$data), permutation.of = reflections$experiment_slots$name)

  expect_class(e$data$task, "Task")
  expect_class(e$data$learner, "Learner")
  if (state >= "trained") {
    expect_class(e$data$resampling, "Resampling")
    expect_int(e$data$iteration, lower = 1L)
    expect_data_table(e$data$train_log, ncol = 2, any.missing = FALSE)
    expect_number(e$data$train_time)
    expect_false(is.null(e$data$model))
  }

  if (state >= "predicted") {
    expect_data_table(e$data$predict_log, ncol = 2, any.missing = FALSE)
    expect_number(e$data$predict_time)
    expect_class(e$data$prediction, "Prediction")
    expect_atomic_vector(e$data$prediction$response, len = length(e$test_set), any.missing = FALSE)
  }

  if (state >= "scored") {
    expect_list(e$data$performance, names = "unique")
    qassertr(e$data$performance, "N1")
  }
}

expect_resample_result = function(rr) {
  expect_r6(rr, "ResampleResult")
  expect_task(rr$task)
  expect_learner(rr$learner, task = rr$task)
  expect_resampling(rr$resampling, task = rr$task)

  data = rr$data
  expect_data_table(rr$data, nrow = rr$resampling$iters, ncol = nrow(reflections$experiment_slots), any.missing = FALSE)
  expect_names(names(rr$data), permutation.of = reflections$experiment_slots$name)
  expect_hash(rr$hash)

  perf = rr$performance
  expect_data_table(perf, nrow = rr$resampling$iters, min.cols = 2L)
  expect_names(names(perf), must.include = c("iteration", ids(rr$task$measures)))
  expect_identical(perf$iteration, seq_len(rr$resampling$iters))
  for (m in names(rr$task$measures))
    expect_numeric(perf[[m]], any.missing = FALSE)

  e = rr$experiment(1L)
  expect_experiment(e)
  expect_true(e$state == "scored")

  measures = rr$measures
  aggr = rr$aggregated
  for (m in measures) {
    expect_number(aggr[[m$id]], lower = m$range[1L], upper = m$range[2L], label = sprintf("measure %s", m$id))
  }
}

expect_benchmark_result = function(bmr) {
  expect_r6(bmr, "BenchmarkResult", public = c("data", "resample_results", "resample_result", "performance"))

  rrs = bmr$resample_results
  expect_data_table(rrs, ncol = 5L)
  expect_names(names(rrs), permutation.of = c("task", "learner", "resampling", "hash", "N"))
  expect_character(rrs$hash, any.missing = FALSE, unique = TRUE)
  expect_integer(rrs$N, any.missing = FALSE, lower = 1L)
}

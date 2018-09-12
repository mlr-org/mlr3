library(checkmate)
library(testthat)

future::plan("multiprocess")

`[[.R6` = function(x, i, ...) {
  if (!backports:::hasName(x, i))
    stop("R6 class ", paste0(class(x), collapse = "/") ," does not have slot '", i, "'!")
  get(i, envir = x)
}

`$.R6` = function(x, name) {
  if (!backports:::hasName(x, name))
    stop("R6 class ", paste0(class(x), collapse = "/") ," does not have slot '", name, "'!")
  get(name, envir = x)
}

private = function(x) {
  if (!R6::is.R6(x))
    stop("Expected R6 class")
  x$.__enclos_env__[["private"]]
}

expect_same_address = function(x, y) {
  testthat::expect_identical(data.table::address(x), data.table::address(y))
}

expect_different_address = function(x, y) {
  testthat::expect_false(identical(data.table::address(x), data.table::address(y)))
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

  # rows are duplicated
  x = b$data(rows = rep(rn[1L], 2L), cols = b$colnames)
  expect_data_table(x, nrow = 2L, ncol = p)

  # duplicated cols raise exception
  expect_error(b$data(rows = rn[1L], cols = rep(cn[1L], 2L)), "uniquely")

  expect_data_table(b$head(3), nrow = 3, ncol = p)
}

expect_task = function(task) {
  expect_r6(task, "Task", cloneable = TRUE, public = c("id", "backend", "row_info", "col_info", "order", "head", "row_ids", "feature_names", "target_names", "formula", "nrow", "ncol", "col_types"))
  expect_string(task$id, min.chars = 1L)
  expect_count(task$nrow)
  expect_count(task$ncol)
  expect_data_table(task$data())
  expect_data_table(task$head(1), nrow = 1L)

  cols = c("id", "role", "type")
  expect_data_table(task$col_info, key = "id", ncol = length(cols))
  expect_names(names(task$col_info), permutation.of = cols)
  expect_character(task$col_info$id, any.missing = FALSE, unique = TRUE)
  expect_subset(task$col_info$role, capabilities$task_col_roles, fmatch = TRUE)
  expect_subset(task$col_info$type, capabilities$task_col_types, fmatch = TRUE)

  cols = c("id", "role")
  expect_data_table(task$row_info, key = "id", ncol = length(cols))
  expect_names(names(task$row_info), permutation.of = cols)
  expect_atomic_vector(task$row_info$id, any.missing = FALSE, unique = TRUE)
  expect_subset(task$row_info$role, capabilities$task_row_roles, fmatch = TRUE)

  types = task$col_types
  expect_data_table(types, ncol = 2, nrow = task$ncol)
  expect_set_equal(types$id, c(task$target_names, task$feature_names))
  expect_subset(types$type, capabilities$task_col_types, fmatch = TRUE)

  expect_character(task$order, any.missing = FALSE)
  expect_names(task$order, subset.of = c(task$feature_names, task$target_names))
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
}

expect_task_regr = function(task) {
  expect_r6(task, "TaskRegr")
  y = task$truth()
  expect_data_table(y, ncol = 1)
  y = y[[1]]
  expect_numeric(y, any.missing = FALSE)
}

expect_learner = function(lrn, task = NULL) {
  expect_r6(lrn, "Learner", cloneable = TRUE)
  expect_output(print(lrn))

  expect_choice(lrn$task_type, capabilities$task_types, fmatch = TRUE)
  expect_character(lrn$packages, any.missing = FALSE, min.chars = 1L)
  expect_class(lrn$par_set, "ParamSet")
  expect_subset(lrn$properties, capabilities$learner_props[[class(task)[1L]]])
  expect_function(lrn$train, args = c("task", "..."), ordered = TRUE)
  expect_function(lrn$predict, args = c("task", "..."), ordered = TRUE)

  if (!is.null(task)) {
    assert_class(task, "Task")
    expect_identical(lrn$task_type, class(task)[1L])
  }
}

expect_resampling = function(r, task = NULL) {
  expect_r6(r, "Resampling")
  expect_string(r$id, min.chars = 1L)

  instance = private(r)$.instance
  if (is.null(instance)) {
    expect_false(r$is_instantiated)
    expect_error(r$train_set(1L), "instantiated")
    expect_error(r$test_set(1L), "instantiated")
    if (r$id != "custom")
      expect_count(r$iters, positive = TRUE)
  } else {
    expect_true(r$is_instantiated)
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
        expect_subset(train, ids, fmatch = TRUE)
        expect_subset(train, ids, fmatch = TRUE)
      }
    }
  }
  expect_list(r$par_vals, names = "unique")
  expect_true(qtestr(r$par_vals, "V1"))
}

expect_measure = function(m) {
  expect_r6(m, "Measure")
  expect_string(m$id, min.chars = 1L)
  expect_subset(m$task_types, capabilities$task_types, empty.ok = FALSE)
  expect_numeric(m$range, len = 2, any.missing = FALSE)
  expect_lt(m$range[1], m$range[2])
  expect_flag(m$minimize)
  expect_character(m$packages, min.chars = 1L, any.missing = FALSE, unique = TRUE)
  expect_function(m$calculate, args = "experiment")
}

expect_experiment = function(e) {
  expect_r6(e, "Experiment")
  state = e$state
  expect_factor(state, ordered = TRUE)
  expect_subset(as.character(state), capabilities$experiment_states, fmatch = TRUE)
  expect_list(e$data, len = nrow(reflections$experiment_slots))
  expect_names(names(e$data), permutation.of = reflections$experiment_slots$name)

  expect_class(e$data$task, "Task")
  expect_class(e$data$learner, "Learner")
  if (state >= "trained") {
    expect_class(e$data$resampling, "Resampling")
    expect_int(e$data$iteration, lower = 1L)
    expect_data_table(e$data$train_log, ncol = 2, any.missing = FALSE)
    expect_number(e$data$train_time)
    expect_false(is.null(e$data$learner$model))
  }

  if (state >= "predicted") {
    expect_data_table(e$data$test_log, ncol = 2, any.missing = FALSE)
    expect_number(e$data$test_time)
    expect_atomic_vector(e$data$predicted, len = length(e$test_set))
  }

  if (state >= "scored") {
    expect_list(e$data$performance, names = "unique")
    qassertr(e$data$performance, "N1")
  }
}

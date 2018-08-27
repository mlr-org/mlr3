library(checkmate)
library(testthat)
library(stringi)

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

expect_backend = function(backends) {
  if (inherits(backends, "Backend"))
    backends = list(backends)
  expect_list(backends, min.len = 1L, types = "Backend")

  for (b in backends) {
    expect_r6(b, cloneable = TRUE, public = c("nrow", "ncol", "colnames", "rownames", "head", "data"))
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
}

expect_task = function(task) {
  expect_r6(task, "Task", cloneable = TRUE, public = c("task_type", "id", "backend", "rows", "cols", "order", "head", "row_ids", "features", "target", "formula", "nrow", "ncol", "col_types"))
  expect_string(task$id, min.chars = 1L)
  expect_count(task$nrow)
  expect_count(task$ncol)
  expect_data_table(task$data())
  expect_data_table(task$head(1), nrow = 1L)

  cols = c("id", "role", "type")
  expect_data_table(task$cols, key = "id", ncol = length(cols))
  expect_names(names(task$cols), permutation.of = cols)
  expect_character(task$cols$id, any.missing = FALSE, unique = TRUE)
  expect_subset(task$cols$role, capabilities$task_col_roles, fmatch = TRUE)
  expect_subset(task$cols$type, capabilities$task_col_types, fmatch = TRUE)

  cols = c("id", "role")
  expect_data_table(task$rows, key = "id", ncol = length(cols))
  expect_names(names(task$rows), permutation.of = cols)
  expect_atomic_vector(task$rows$id, any.missing = FALSE, unique = TRUE)
  expect_subset(task$rows$role, capabilities$task_row_roles, fmatch = TRUE)

  types = task$col_types
  expect_data_table(types, ncol = 2, nrow = task$ncol)
  expect_set_equal(types$id, c(task$target_names, task$features))
  expect_subset(types$type, capabilities$task_col_types, fmatch = TRUE)

  expect_character(task$blocking, any.missing = FALSE)
  expect_names(task$blocking, subset.of = c(task$features, task$target_names))

  expect_character(task$order, any.missing = FALSE)
  expect_names(task$order, subset.of = c(task$features, task$target_names))
}

expect_task_supervised = function(task) {
  expect_r6(task, "TaskSupervised", cloneable = TRUE)
  expect_choice(task$target_names, task$cols$id)

  expect_class(task$formula, "formula")
  tf = terms(task$formula)
  expect_set_equal(labels(tf), task$features) # rhs
  expect_set_equal(setdiff(all.vars(tf), labels(tf)), task$target_names) # lhs
  expect_subset(task$features, colnames(task$head()))
}

expect_task_classif = function(task) {
  expect_r6(task, "TaskClassif")
  y = task$truth()
  expect_data_table(y, ncol = 1)
  expect_data_table(y, ncol = 1)
  y = y[[1]]
  expect_true(is.character(y) || is.factor(y))

  expect_int(task$nclasses, lower = 2L)
  expect_equal(task$nclasses, length(unique(y)))
  expect_character(task$classes, any.missing = FALSE)
  expect_subset(task$classes, as.character(y))
  if (task$nclasses > 2L)
    expect_identical(task$positive, NA_character_)
  else
    expect_choice(task$positive, task$classes)
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
  expect_true(stri_startswith_fixed(lrn$id, lrn$task_type))
  expect_character(lrn$packages, any.missing = FALSE, min.chars = 1L)
  expect_class(lrn$par_set, "ParamSet")
  expect_subset(lrn$properties, capabilities$learner_props[[task$task_type]])
  expect_function(lrn$train, args = c("task", "row_ids"), ordered = TRUE)
  expect_function(lrn$predict, args = c("model", "task", "row_ids"), ordered = TRUE)

  if (!is.null(task)) {
    assert_r6(task, "Task")
    expect_identical(lrn$task_type, task$task_type)
  }
}

expect_resampling = function(r, task = NULL) {
  expect_r6(r, "Resampling")
  expect_string(r$id)

  instance = private(r)$instance
  if (is.null(instance)) {
    expect_error(r$train_set(1L), "instantiated")
    expect_error(r$test_set(1L), "instantiated")
  } else {
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
}

expect_experiment = function(e) {
  expect_r6(e, "Experiment")
  state = e$state
  expect_factor(state, ordered = TRUE)
  expect_subset(as.character(state), capabilities$experiment_states, fmatch = TRUE)
  expect_list(e$data, len = nrow(capabilities$experiment_slots))
  expect_names(names(e$data), permutation.of = capabilities$experiment_slots$name)

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
    expect_data_table(e$data$test_log, ncol = 2, any.missing = FALSE)
    expect_number(e$data$test_time)
    expect_atomic_vector(e$data$predicted, len = length(e$test_set))
  }

  if (state >= "scored") {
    expect_list(e$data$performance, names = "unique")
    qassertr(e$data$performance, "N1")
  }
}

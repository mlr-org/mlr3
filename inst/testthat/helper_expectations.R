expect_man_exists = function(man) {
  checkmate::expect_string(man, na.ok = TRUE, fixed = "::")
  if (!is.na(man)) {
    parts = strsplit(man, "::", fixed = TRUE)[[1L]]
    matches = help.search(parts[2L], package = parts[1L], ignore.case = FALSE)
    checkmate::expect_data_frame(matches$matches, min.rows = 1L)
  }
}
expect_same_address = function(x, y) {
  requireNamespace("data.table")
  testthat::expect_identical(data.table::address(x), data.table::address(y))
}

expect_different_address = function(x, y) {
  requireNamespace("data.table")
  testthat::expect_false(identical(data.table::address(x), data.table::address(y)))
}

expect_id = function(x, len = NULL) {
  checkmate::expect_character(x, len = len, any.missing = FALSE, min.chars = 1L, unique = FALSE)
}

expect_hash = function(x, len = NULL) {
  pattern = "^[0-9a-z]{16}$"
  checkmate::expect_character(x, len = len, any.missing = FALSE, pattern = pattern, unique = TRUE)
}

expect_uhash = function(x, len = NULL) {
  pattern = "^[a-z0-9]{8}-[a-z0-9]{4}-[a-z0-9]{4}-[a-z0-9]{4}-[a-z0-9]{12}$"
  checkmate::expect_character(x, len = len, any.missing = FALSE, pattern = pattern, unique = TRUE)
}

expect_dictionary = function(d, contains = NA_character_, min_items = 0L) {
  checkmate::expect_r6(d, "Dictionary")
  testthat::expect_output(print(d), "Dictionary")
  keys = d$keys()

  checkmate::expect_environment(d$items)
  checkmate::expect_character(keys, any.missing = FALSE, min.len = min_items, min.chars = 1L)
  if (!is.na(contains)) {
    checkmate::expect_list(d$mget(keys), types = contains, names = "unique")
  }
  if (length(keys) >= 1L) {
    expect_error(d$get(keys[1], 1), "names")
  }
  expect_data_table(data.table::as.data.table(d), key = "key", nrows = length(keys))
}

expect_backend = function(b) {
  checkmate::expect_r6(b, cloneable = FALSE,
    public = c("nrow", "ncol", "colnames", "rownames", "head", "data", "hash"),
    private = c(".data", ".hash", ".calculate_hash"))
  checkmate::expect_subset(b$data_formats, mlr3::mlr_reflections$data_formats, empty.ok = FALSE)
  testthat::expect_output(print(b), "^<DataBackend")

  n = checkmate::expect_count(b$nrow)
  p = checkmate::expect_count(b$ncol)
  cn = checkmate::expect_atomic_vector(b$colnames, any.missing = FALSE, len = p, unique = TRUE)
  rn = checkmate::expect_atomic_vector(b$rownames, any.missing = FALSE, len = n, unique = TRUE)
  rn1 = head(rn, 1L)
  pk = b$primary_key

  x = b$data(rows = rn, cols = pk, data_format = "data.table")
  checkmate::expect_data_table(x, ncols = 1L, nrows = n, col.names = "unique")
  x = x[[1L]]
  checkmate::expect_atomic_vector(x, len = n, unique = TRUE)

  x = b$data(rows = rn, cols = setdiff(cn, pk)[1L], data_format = "data.table")
  checkmate::expect_data_table(x, ncols = 1L, nrows = n, col.names = "unique")
  x = x[[1L]]
  checkmate::expect_atomic_vector(x, len = n)

  # extra cols are ignored
  x = b$data(rows = rn1, cols = c(cn[1L], "_not_existing_"), data_format = "data.table")
  checkmate::expect_data_table(x, nrows = length(rn1), ncols = 1L)

  # zero cols matching
  x = b$data(rows = rn1, cols = "_not_existing_", data_format = "data.table")
  checkmate::expect_data_table(x, nrows = 0L, ncols = 0L)

  # extra rows are ignored
  query_rows = c(rn1, if (is.integer(rn)) -1L else "_not_existing_")
  x = b$data(query_rows, cols = cn[1L], data_format = "data.table")
  checkmate::expect_data_table(x, nrows = length(rn1), ncols = 1L)

  # zero rows matching
  query_rows = if (is.integer(rn)) -1L else "_not_existing_"
  x = b$data(rows = query_rows, cols = cn[1L], data_format = "data.table")
  checkmate::expect_data_table(x, nrows = 0L, ncols = 1L)

  # rows are duplicated
  x = b$data(rows = rep(rn1, 2L), cols = b$colnames, data_format = "data.table")
  checkmate::expect_data_table(x, nrows = 2L*length(rn1), ncols = p)

  # rows are returned in the right order
  i = sample(rn, min(n, 10L))
  x = b$data(rows = i, cols = b$primary_key, data_format = "data.table")
  testthat::expect_equal(i, x[[1L]])

  # duplicated cols raise exception
  testthat::expect_error(b$data(rows = rn1, cols = rep(cn[1L], 2L), data_format = "data.table"), "unique")

  # $head()
  checkmate::expect_data_table(b$head(9999L), nrows = n, ncols = p)
  checkmate::expect_data_table(b$head(0L), nrows = 0, ncols = p)
  checkmate::expect_data_table(b$head(2L), nrows = min(2L, b$nrow), ncols = p)

  # $distinct()
  d = b$distinct(rn, b$primary_key)[[1L]]
  checkmate::expect_atomic_vector(d, any.missing = FALSE, len = n, unique = TRUE)
  checkmate::expect_list(b$distinct(rn, "_not_existing_"), len = 0L, names = "named")
  d = b$distinct(rn, c("_not_existing_", rev(cn), "_also_not_existing_"))
  checkmate::expect_list(d, names = "unique")
  testthat::expect_equal(names(d), rev(cn))

  d = b$distinct(rn1, cn)
  expect_list(d, len = length(cn), names = "unique", any.missing = FALSE)
  expect_true(all(lengths(d) <= 1L)) # NA -> 0 zero length

  ## missings are handled by distinct?
  d = b$distinct(rn, cn, na_rm = TRUE)
  checkmate::qexpectr(d, "V")

  d = b$distinct(rn, cn, na_rm = FALSE)
  m = b$missings(rn, cn)
  expect_equal(sapply(d, checkmate::anyMissing), m > 0L)

  # $missings()
  x = b$missings(b$rownames, b$colnames)
  checkmate::expect_integer(x, lower = 0L, upper = b$nrow, any.missing = FALSE)
  checkmate::expect_names(names(x), permutation.of = b$colnames)
  checkmate::expect_integer(b$missings(b$rownames, "_not_existing_"), len = 0L, names = "named")
  checkmate::expect_integer(b$missings(b$rownames[0L], b$colnames), len = b$ncol, names = "unique")
  checkmate::expect_integer(b$missings(b$rownames[0L], "_not_existing_"), len = 0L, names = "unique")

  # $hash
 checkmate::expect_string(b$hash)
}

expect_iris_backend = function(b, n_missing = 0L) {
  testthat::expect_equal(b$nrow, 150L)
  testthat::expect_equal(b$ncol, 6L)
  checkmate::expect_set_equal(b$colnames, c(names(iris), b$primary_key))
  checkmate::expect_set_equal(b$rownames, seq_len(150L))

  x = b$head(2)
  checkmate::expect_data_table(x, nrows  = 2L, ncols  = 6L, any.missing = FALSE)
  checkmate::expect_set_equal(names(x), c(names(iris), b$primary_key))

  x = b$distinct(b$rownames, "Species")
  checkmate::expect_list(x, "character", len = 1)
  checkmate::expect_set_equal(x$Species, levels(iris$Species))

  x = b$data(rows = 2:10, cols = c(b$primary_key, "Species", "Sepal.Width"), data_format = "data.table")
  checkmate::expect_data_table(x, nrows  = 9L, ncols  = 3L)
  checkmate::expect_set_equal(names(x), c(b$primary_key, "Species", "Sepal.Width"))
  checkmate::expect_set_equal(x[[b$primary_key]], 2:10)

  if (is.factor(x$Species)) {
    checkmate::expect_factor(x$Species, levels = levels(iris$Species))
  } else {
    checkmate::expect_character(x$Species, any.missing = FALSE)
  }

  testthat::expect_true(all(x$Species == "setosa"))
  checkmate::expect_numeric(x$Sepal.Width, any.missing = FALSE)

  # invalid row ids
  x = b$data(rows = 150:151, cols = c(b$primary_key, "Species"), data_format = "data.table")
  checkmate::expect_data_table(x, nrows  = 1L, ncols  = 2L)
  testthat::expect_equal(x[[b$primary_key]], 150L)
  testthat::expect_equal(as.character(x[["Species"]]), "virginica")

  # duplicated row ids
  x = b$data(rows = c(1L, 1L), cols = c(b$primary_key, "Species"), data_format = "data.table")
  checkmate::expect_data_table(x, nrows  = 2L, ncols  = 2L)
  testthat::expect_equal(x[[b$primary_key]], c(1L, 1L))
  testthat::expect_equal(as.character(x[["Species"]]), c("setosa", "setosa"))

  # no missing values
  x = b$missings(b$rownames, b$colnames)
  checkmate::expect_integer(x, names = "unique", lower = 0L, upper = b$nrow, any.missing = FALSE)
  checkmate::expect_names(names(x), permutation.of = b$colnames)
  testthat::expect_identical(sum(x), as.integer(n_missing))
}

expect_task = function(task) {
  checkmate::expect_r6(task, "Task", cloneable = TRUE, public = c("id", "backend", "task_type", "row_roles", "col_roles", "col_info", "head", "row_ids", "feature_names", "target_names", "formula", "nrow", "ncol", "feature_types"))
  testthat::expect_output(print(task), "Task")
  expect_id(task$id)
  expect_man_exists(task$man)
  checkmate::expect_count(task$nrow)
  checkmate::expect_count(task$ncol)
  checkmate::expect_data_table(task$data(data_format = "data.table"))
  if (task$nrow > 0L)
    checkmate::expect_data_table(task$head(1), nrows = 1L)

  cols = c("id", "type", "levels")
  checkmate::expect_data_table(task$col_info, key = "id", ncols  = length(cols))
  checkmate::expect_names(names(task$col_info), permutation.of = cols)
  expect_id(task$col_info$id)
  checkmate::expect_subset(task$col_info$type, mlr3::mlr_reflections$task_feature_types)
  checkmate::expect_list(task$col_info$levels)

  checkmate::expect_list(task$col_roles, names = "unique", any.missing = FALSE)
  checkmate::expect_names(names(task$col_roles), permutation.of = mlr3::mlr_reflections$task_col_roles[[task$task_type]])
  lapply(task$col_roles, checkmate::expect_character, any.missing = FALSE, unique = TRUE, min.chars = 1L)
  checkmate::expect_subset(unlist(task$col_roles, use.names = FALSE), task$col_info$id)

  checkmate::expect_list(task$col_roles_by_name, names = "unique", any.missing = FALSE)
  checkmate::expect_names(names(task$col_roles_by_name), permutation.of = setdiff(task$col_info$id, task$backend$primary_key))
  lapply(task$col_roles_by_name, checkmate::expect_character, any.missing = FALSE, unique = TRUE, min.chars = 1L)
  checkmate::expect_subset(unlist(task$col_roles_by_name, use.names = FALSE), mlr3::mlr_reflections$task_col_roles[[task$task_type]])


  checkmate::expect_list(task$row_roles, names = "unique", types = c("integer", "character"), any.missing = FALSE)
  checkmate::expect_names(names(task$row_roles), permutation.of = mlr3::mlr_reflections$task_row_roles)
  lapply(task$row_roles, checkmate::expect_atomic_vector, any.missing = FALSE, unique = TRUE)

  types = task$feature_types
  checkmate::expect_data_table(types, ncols  = 2, nrows  = length(task$feature_names), key = "id")
  checkmate::expect_set_equal(types$id, task$feature_names)
  checkmate::expect_subset(types$type, mlr3::mlr_reflections$task_feature_types)

  properties = task$properties
  checkmate::expect_subset(properties, mlr3::mlr_reflections$task_properties[[task$task_type]])

  levels = task$levels()
  checkmate::expect_list(levels, names = "unique")
  checkmate::qassertr(levels, c("0", "S+"))

  missings = task$missings()
  checkmate::expect_integer(missings, names = "unique", any.missing = FALSE, lower = 0L, upper = task$nrow)

  expect_hash(task$hash, 1L)

  # query zero columns
  data = task$data(cols = character(), data_format = "data.table")
  checkmate::expect_data_table(data, ncols  = 0L)

  # query zero rows
  data = task$data(rows = task$row_ids[0L], data_format = "data.table")
  checkmate::expect_data_table(data, nrows  = 0L)
}

expect_task_supervised = function(task) {
  checkmate::expect_r6(task, "TaskSupervised", cloneable = TRUE)
  checkmate::expect_subset(task$target_names, task$col_info$id, empty.ok = FALSE)

  f = task$formula()
  checkmate::expect_class(f, "formula")
  # tf = terms(f)
  # checkmate::expect_set_equal(labels(tf), task$feature_names) # rhs
  # checkmate::expect_set_equal(setdiff(all.vars(tf), labels(tf)), task$target_names) # lhs
  checkmate::expect_subset(task$feature_names, colnames(task$head()))
  expect_hash(task$hash, 1L)
}

expect_task_classif = function(task) {
  checkmate::expect_r6(task, "TaskClassif")
  y = task$truth()
  checkmate::expect_factor(y)

  testthat::expect_gte(length(task$class_names), 2L)
  checkmate::expect_character(task$class_names, any.missing = FALSE)
  checkmate::expect_subset(task$class_names, as.character(y))
  if (length(task$class_names) > 2L) {
    testthat::expect_identical(task$positive, NA_character_)
    testthat::expect_identical(task$negative, NA_character_)
  } else {
    checkmate::expect_set_equal(c(task$positive, task$negative), task$class_names)
    testthat::expect_true(task$positive != task$negative)
  }
  expect_hash(task$hash, 1L)
}

expect_task_regr = function(task) {
  checkmate::expect_r6(task, "TaskRegr")
  y = task$truth()
  checkmate::expect_numeric(y, any.missing = FALSE)
  expect_hash(task$hash, 1L)
}

expect_task_generator = function(gen) {
  checkmate::expect_r6(gen, "TaskGenerator", private = ".generate")
  expect_id(gen$id)
  expect_man_exists(gen$man)
  checkmate::expect_choice(gen$task_type, mlr3::mlr_reflections$task_types$type)
  checkmate::expect_function(gen$generate, args = "n")
  checkmate::expect_class(gen$param_set, "ParamSet")
  checkmate::expect_list(gen$param_set$values, names = "unique")
}

expect_learner = function(lrn, task = NULL) {
  checkmate::expect_r6(lrn, "Learner", cloneable = TRUE)
  expect_id(lrn$id)
  expect_man_exists(lrn$man)
  testthat::expect_output(print(lrn))

  checkmate::expect_choice(lrn$task_type, mlr3::mlr_reflections$task_types$type)
  checkmate::expect_character(lrn$packages, any.missing = FALSE, min.chars = 1L, unique = TRUE)
  checkmate::expect_class(lrn$param_set, "ParamSet")
  checkmate::expect_character(lrn$properties, any.missing = FALSE, min.chars = 1L, unique = TRUE)
  checkmate::expect_function(lrn$train_internal, args = "task", nargs = 1L)
  checkmate::expect_function(lrn$predict_internal, args = "task", nargs = 1L)
  expect_hash(lrn$hash, 1L)

  tags = lrn$param_set$tags
  tags = Filter(function(tags) !any(c("train", "predict") %in% tags), tags)
  testthat::expect_true(length(tags) == 0L,
    info = sprintf("All hyperparameters of learner %s must be tagged with 'train' or 'predict'. Missing tags for: %s", lrn$id, paste0(names(tags), collapse = ", "))
  )

  if (!is.null(task)) {
    checkmate::expect_class(task, "Task")
    checkmate::expect_subset(lrn$properties, mlr3::mlr_reflections$learner_properties[[task$task_type]])
    testthat::expect_identical(lrn$task_type, task$task_type)
  }
}

expect_resampling = function(r, task = NULL) {
  checkmate::expect_r6(r, "Resampling")
  testthat::expect_output(print(r), "Resampling")
  expect_id(r$id)
  expect_man_exists(r$man)

  instance = r$instance
  if (is.null(instance)) {
    testthat::expect_false(r$is_instantiated)
    testthat::expect_error(r$train_set(1L), "instantiated")
    testthat::expect_error(r$test_set(1L), "instantiated")
    # testthat::expect_identical(r$hash, NA_character_)
    if (r$id != "custom")
      checkmate::expect_count(r$iters, positive = TRUE)
    testthat::expect_identical(r$task_hash, NA_character_)
  } else {
    testthat::expect_true(r$is_instantiated)
    expect_hash(r$hash, 1L)
    expect_hash(r$task_hash, 1L)
    if (!is.null(task)) {
      ids = task$row_ids
      testthat::expect_equal(task$hash, r$task_hash)
    }
    checkmate::expect_count(r$iters, positive = TRUE)

    for (i in seq_len(r$iters)) {
      train = r$train_set(1L)
      test = r$test_set(1L)
      checkmate::expect_atomic_vector(train, any.missing = FALSE)
      checkmate::expect_atomic_vector(test, any.missing = FALSE)
      testthat::expect_length(intersect(train, test), 0L)
      if (!is.null(task)) {
        checkmate::expect_subset(train, ids)
        checkmate::expect_subset(train, ids)
      }
    }
  }
  checkmate::expect_list(r$param_set$values, names = "unique")
  testthat::expect_true(checkmate::qtestr(r$param_set$values, "V1"))
}

expect_measure = function(m) {
  checkmate::expect_r6(m, "Measure", public = c("aggregate", "score", "score_internal", "id", "minimize", "packages", "range", "task_type", "task_properties"))
  expect_id(m$id)
  expect_man_exists(m$man)
  testthat::expect_output(print(m), "Measure")

  expect_id(m$id)
  checkmate::expect_subset(m$task_type, c(NA_character_, mlr3::mlr_reflections$task_types$type), empty.ok = FALSE)
  checkmate::expect_numeric(m$range, len = 2, any.missing = FALSE)
  testthat::expect_lt(m$range[1], m$range[2])
  checkmate::expect_flag(m$minimize, na.ok = TRUE)
  checkmate::expect_character(m$packages, min.chars = 1L, any.missing = FALSE, unique = TRUE)
  checkmate::expect_function(m$score_internal, args = c("prediction", "..."))
  checkmate::expect_function(m$aggregate, args = "rr")
}

expect_prediction = function(p) {
  checkmate::expect_r6(p, "Prediction", public = c("row_ids", "truth", "predict_types"))
  testthat::expect_output(print(p), "^<Prediction")
  checkmate::expect_data_table(data.table::as.data.table(p), nrows  = length(p$row_ids))
  checkmate::expect_atomic_vector(p$missing)
}

expect_prediction_regr = function(p) {
  expect_prediction(p)
  checkmate::expect_r6(p, "PredictionRegr", public = c("row_ids", "response", "truth", "predict_types", "se"))
  checkmate::expect_numeric(p$truth, any.missing = TRUE, len = length(p$row_ids), null.ok = TRUE)
  checkmate::expect_numeric(p$response, any.missing = FALSE, len = length(p$row_ids), null.ok = TRUE)
  if ("se" %in% p$predict_types) {
    checkmate::expect_numeric(p$se, any.missing = FALSE, len = length(p$row_ids), lower = 0)
  }
}

expect_prediction_classif = function(p, task = NULL) {
  expect_prediction(p)
  checkmate::expect_r6(p, "PredictionClassif", public = c("row_ids", "response", "truth", "predict_types", "prob"))
  n = length(p$row_ids)
  lvls = if (is.null(task)) NULL else task$class_names
  checkmate::expect_factor(p$truth, len = n, levels = lvls, null.ok = TRUE)
  checkmate::expect_factor(p$response, len = n, levels = lvls, null.ok = TRUE)
  if ("prob" %in% p$predict_types) {
    checkmate::expect_matrix(p$prob, "numeric", any.missing = FALSE, ncols = nlevels(p$response), nrows = n)
  }
  confusion = p$confusion
  checkmate::expect_matrix(confusion)
  checkmate::expect_integer(confusion, lower = 0L, any.missing = FALSE)
  if (!is.null(task)) {
    checkmate::expect_names(rownames(confusion), identical.to = task$class_names)
    checkmate::expect_names(colnames(confusion), identical.to = task$class_names)
  }
}

expect_resample_result = function(rr) {
  checkmate::expect_r6(rr, "ResampleResult")
  testthat::expect_output(print(rr), "ResampleResult")
  expect_task(rr$task)
  lapply(rr$learners, expect_learner, task = rr$task)
  expect_resampling(rr$resampling, task = rr$task)

  data = data.table::as.data.table(rr)
  checkmate::expect_data_table(rr$score(), nrows  = rr$resampling$iters, min.cols = length(mlr3::mlr_reflections$rr_names), any.missing = FALSE)
  checkmate::expect_names(names(rr$score()), must.include = mlr3::mlr_reflections$rr_names)
  expect_uhash(rr$uhash, 1L)

  m = mlr3::default_measures(rr$task$task_type)[[1L]]
  y = rr$score(m)
  aggr = rr$aggregate(m)
  checkmate::expect_numeric(y[[m$id]], lower = m$range[1], upper = m$range[2], any.missing = FALSE, label = sprintf("measure %s", m$id))
  checkmate::expect_number(aggr[[m$id]], lower = m$range[1L], upper = m$range[2L], label = sprintf("measure %s", m$id))

  lapply(rr$data$prediction, checkmate::expect_list, types = "Prediction", names = "unique")
  expect_prediction(rr$prediction())
  checkmate::expect_list(rr$predictions(), "Prediction")
}

expect_benchmark_result = function(bmr) {
  checkmate::expect_r6(bmr, "BenchmarkResult", public = c("data"))
  testthat::expect_output(print(bmr), "BenchmarkResult")

  checkmate::expect_data_table(bmr$data, min.cols = length(mlr3::mlr_reflections$rr_names) + 1L)
  checkmate::expect_names(names(bmr$data), must.include = c(mlr3::mlr_reflections$rr_names, "uhash"))

  tab = bmr$tasks
  checkmate::expect_data_table(tab, ncols = 3L)
  checkmate::expect_names(names(tab), identical.to = c("task_hash", "task_id", "task"))
  expect_hash(tab$task_hash)
  expect_id(tab$task_id)
  checkmate::expect_list(tab$task, "Task")

  tab = bmr$learners
  checkmate::expect_data_table(tab, ncols = 3L)
  checkmate::expect_names(names(tab), identical.to = c("learner_hash", "learner_id", "learner"))
  expect_hash(tab$learner_hash)
  expect_id(tab$learner_id)
  checkmate::expect_list(tab$learner, "Learner")

  tab = bmr$resamplings
  checkmate::expect_data_table(tab, ncols = 3L)
  checkmate::expect_names(names(tab), identical.to = c("resampling_hash", "resampling_id", "resampling"))
  expect_hash(tab$resampling_hash)
  expect_id(tab$resampling_id)
  checkmate::expect_list(tab$resampling, "Resampling")

  measures = mlr3::default_measures(bmr$task_type)
  tab = bmr$aggregate(measures, ids = TRUE)
  checkmate::expect_data_table(tab, min.cols = 5L + length(measures))
  checkmate::expect_names(names(tab), must.include = c("nr", "resample_result", "resampling_id", "task_id", "learner_id", "resampling_id", mlr3misc::map_chr(measures, "id")))
  testthat::expect_equal(tab$nr, seq_len(nrow(tab)))
  checkmate::expect_list(tab$resample_result, "ResampleResult")
  expect_id(tab$task_id)
  expect_id(tab$learner_id)
  expect_id(tab$resampling_id)
  if (length(measures)) {
    measure = measures[[1L]]
    checkmate::expect_numeric(tab[[measure$id]], any.missing = FALSE)
  }

  tab = bmr$aggregate(params = TRUE)
  checkmate::assert_list(tab$params)

  uhashes = bmr$uhashes
  expect_uhash(uhashes, len = data.table::uniqueN(bmr$data$uhash))
  checkmate::expect_set_equal(uhashes, bmr$data$uhash)

  expect_equal(bmr$n_resample_results, length(bmr$uhashes))

  checkmate::expect_choice(bmr$task_type, mlr3::mlr_reflections$task_types$type, null.ok = nrow(bmr$data) == 0L)
}

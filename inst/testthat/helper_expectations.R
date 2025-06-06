expect_man_exists = function(man) {
  checkmate::expect_string(man, na.ok = TRUE, fixed = "::")
  if (!is.na(man)) {
    parts = strsplit(man, "::", fixed = TRUE)[[1L]]
    matches = help.search(parts[2L], package = parts[1L], ignore.case = FALSE)
    checkmate::expect_data_frame(matches$matches, min.rows = 1L, info = "man page lookup")
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
    testthat::expect_error(d$get(keys[1], 1), "names")
  }
  checkmate::expect_data_table(data.table::as.data.table(d), key = "key", nrows = length(keys))
}

expect_backend = function(b) {
  checkmate::expect_r6(b, cloneable = FALSE,
    public = c("nrow", "ncol", "colnames", "rownames", "head", "data", "hash"),
    private = c(".data", ".hash", ".calculate_hash"))
  testthat::expect_output(print(b), "DataBackend")

  n = checkmate::expect_count(b$nrow)
  p = checkmate::expect_count(b$ncol)
  cn = checkmate::expect_character(b$colnames, any.missing = FALSE, len = p, unique = TRUE)
  rn = checkmate::expect_integerish(b$rownames, any.missing = FALSE, len = n, unique = TRUE)
  rn1 = head(rn, 1L)
  pk = b$primary_key

  x = b$data(rows = rn, cols = pk)
  checkmate::expect_data_table(x, ncols = 1L, nrows = n, col.names = "unique")
  x = x[[1L]]
  checkmate::expect_integerish(x, len = n, unique = TRUE)

  x = b$data(rows = rn, cols = setdiff(cn, pk)[1L])
  checkmate::expect_data_table(x, ncols = 1L, nrows = n, col.names = "unique")
  x = x[[1L]]
  checkmate::expect_atomic_vector(x, len = n)

  # extra cols are ignored
  x = b$data(rows = rn1, cols = c(cn[1L], "_not_existing_"))
  checkmate::expect_data_table(x, nrows = length(rn1), ncols = 1L)

  # zero cols matching
  x = b$data(rows = rn1, cols = "_not_existing_")
  checkmate::expect_data_table(x, nrows = 0L, ncols = 0L)

  # extra rows are ignored
  query_rows = c(rn1, if (is.numeric(rn)) b$nrow + 1L else "_not_existing_")
  x = b$data(query_rows, cols = cn[1L])
  checkmate::expect_data_table(x, nrows = length(rn1), ncols = 1L)

  # zero rows matching
  query_rows = if (is.numeric(rn)) b$nrow + 1L else "_not_existing_"
  x = b$data(rows = query_rows, cols = cn[1L])
  checkmate::expect_data_table(x, nrows = 0L, ncols = 1L)

  # rows are duplicated
  x = b$data(rows = rep(rn1, 2L), cols = b$colnames)
  checkmate::expect_data_table(x, nrows = 2L * length(rn1), ncols = p)

  # cols are returned in the right order
  j = rev(cn)
  x = b$data(rows = rn1, cols = j)
  testthat::expect_equal(j, colnames(x))

  # rows are returned in the right order
  i = sample(rn, min(n, 10L))
  x = b$data(rows = i, cols = b$primary_key)
  testthat::expect_equal(i, x[[1L]])

  # duplicated cols raise exception
  testthat::expect_error(b$data(rows = rn1, cols = rep(cn[1L], 2L)), "unique")

  # $head()
  checkmate::expect_data_table(b$head(.Machine$integer.max), nrows = n, ncols = p)
  checkmate::expect_data_table(b$head(0L), nrows = 0, ncols = p)
  checkmate::expect_data_table(b$head(2L), nrows = min(2L, b$nrow), ncols = p)

  # $distinct()
  d = b$distinct(rn, b$primary_key)[[1L]]
  checkmate::expect_integerish(d, any.missing = FALSE, len = n, unique = TRUE)
  checkmate::expect_list(b$distinct(rn, "_not_existing_"), len = 0L, names = "named")
  d = b$distinct(rn, c("_not_existing_", rev(cn), "_also_not_existing_"))
  checkmate::expect_list(d, names = "unique")
  testthat::expect_equal(names(d), rev(cn))

  d = b$distinct(rn1, cn)
  checkmate::expect_list(d, len = length(cn), names = "unique", any.missing = FALSE)
  testthat::expect_true(all(lengths(d) <= 1L)) # NA -> 0 zero length

  ## missings are handled by distinct?
  d = b$distinct(rn, cn, na_rm = TRUE)
  checkmate::qexpectr(d, "V")

  d = b$distinct(rn, cn, na_rm = FALSE)
  m = b$missings(rn, cn)
  testthat::expect_equal(vapply(d, checkmate::anyMissing, FUN.VALUE = logical(1)), m > 0L)

  # $missings()
  x = b$missings(b$rownames, b$colnames)
  checkmate::expect_integerish(x, lower = 0L, upper = b$nrow, any.missing = FALSE)
  checkmate::expect_names(names(x), permutation.of = b$colnames)
  checkmate::expect_integerish(b$missings(b$rownames, "_not_existing_"), len = 0L, names = "named")
  checkmate::expect_integerish(b$missings(b$rownames[0L], b$colnames), len = b$ncol, names = "unique")
  checkmate::expect_integerish(b$missings(b$rownames[0L], "_not_existing_"), len = 0L, names = "unique")

  # $hash
  checkmate::expect_string(b$hash)

  # col_hashes
  col_hashes = b$col_hashes
  checkmate::expect_character(col_hashes, any.missing = FALSE)
  checkmate::expect_names(names(col_hashes), permutation.of = setdiff(b$colnames, b$primary_key))

  # if multiple cols have the same hash, check that they actually contain the same data.
  realhashes = sapply(b$data(rows = b$rownames, cols = setdiff(b$colnames, b$primary_key)), mlr3misc::calculate_hash)
  expect_equal(unname(sapply(split(realhashes, col_hashes), data.table::uniqueN)), rep(1, data.table::uniqueN(col_hashes)))
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

  x = b$data(rows = 2:10, cols = c(b$primary_key, "Species", "Sepal.Width"))
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
  x = b$data(rows = 150:151, cols = c(b$primary_key, "Species"))
  checkmate::expect_data_table(x, nrows  = 1L, ncols  = 2L)
  testthat::expect_equal(x[[b$primary_key]], 150L)
  testthat::expect_equal(as.character(x[["Species"]]), "virginica")

  # duplicated row ids
  x = b$data(rows = c(1L, 1L), cols = c(b$primary_key, "Species"))
  checkmate::expect_data_table(x, nrows  = 2L, ncols  = 2L)
  testthat::expect_equal(x[[b$primary_key]], c(1L, 1L))
  testthat::expect_equal(as.character(x[["Species"]]), c("setosa", "setosa"))

  # no missing values
  x = b$missings(b$rownames, b$colnames)
  checkmate::expect_integerish(x, names = "unique", lower = 0L, upper = b$nrow, any.missing = FALSE)
  checkmate::expect_names(names(x), permutation.of = b$colnames)
  testthat::expect_equal(sum(x), n_missing)
}

expect_task = function(task, null_backend_ok = TRUE, duplicated_ids = FALSE) {
  checkmate::expect_r6(task, "Task", cloneable = TRUE, public = c("id", "backend", "task_type", "row_roles", "col_roles", "col_info", "head", "row_ids", "feature_names", "target_names", "formula", "nrow", "ncol", "feature_types"))
  testthat::expect_output(print(task), "Task")
  expect_id(task$id)
  checkmate::expect_string(task$label, na.ok = TRUE)
  expect_man_exists(task$man)
  checkmate::expect_count(task$nrow)
  checkmate::expect_count(task$ncol)

  null_backend = is.null(task$backend)
  if (!null_backend_ok) {
    testthat::expect_false(is.null(task$backend))
  }

  if (task$nrow > 0L && !null_backend) {
    checkmate::expect_data_table(head(task, 1), nrows = 1L)
  }

  cols = c("id", "type", "levels", "label", "fix_factor_levels")
  checkmate::expect_data_table(task$col_info, key = "id", ncols  = length(cols))
  checkmate::expect_names(names(task$col_info), permutation.of = cols)
  expect_id(task$col_info$id)
  checkmate::expect_subset(task$col_info$type, mlr3::mlr_reflections$task_feature_types)
  checkmate::expect_list(task$col_info$levels)
  checkmate::expect_character(task$col_info$label)
  checkmate::expect_logical(task$col_info$fix_factor_levels)

  checkmate::expect_list(task$col_roles, names = "unique", any.missing = FALSE)
  checkmate::expect_names(names(task$col_roles), permutation.of = mlr3::mlr_reflections$task_col_roles[[task$task_type]])
  lapply(task$col_roles, checkmate::expect_character, any.missing = FALSE, unique = TRUE, min.chars = 1L)
  checkmate::expect_subset(unlist(task$col_roles, use.names = FALSE), task$col_info$id)

  checkmate::expect_list(task$row_roles, names = "unique", types = c("integer", "character", "numeric"), any.missing = FALSE)
  checkmate::expect_names(names(task$row_roles), permutation.of = mlr3::mlr_reflections$task_row_roles)
  lapply(task$row_roles, checkmate::expect_integerish, any.missing = FALSE, unique = !duplicated_ids)

  types = task$feature_types
  checkmate::expect_data_table(types, ncols  = 2, nrows  = length(task$feature_names), key = "id")
  checkmate::expect_set_equal(types$id, task$feature_names)
  checkmate::expect_subset(types$type, mlr3::mlr_reflections$task_feature_types)

  properties = task$properties
  checkmate::expect_subset(properties, mlr3::mlr_reflections$task_properties[[task$task_type]])

  levels = task$levels()
  checkmate::expect_list(levels, names = "unique")
  checkmate::qassertr(levels, c("0", "S+"))

  expect_hash(task$hash, 1L)

  if (!null_backend) {
    missings = task$missings()
    checkmate::expect_integer(missings, names = "unique", any.missing = FALSE, lower = 0L, upper = task$nrow)

    missings = task$missings(character())
    checkmate::expect_integer(missings, len = 0L)
    testthat::expect_named(missings)

    # query zero columns
    data = task$data(cols = character())
    checkmate::expect_data_table(data, ncols  = 0L)

    # query zero rows
    data = task$data(rows = task$row_ids[0L])
    checkmate::expect_data_table(data, nrows  = 0L)
  }

  # col_hashes
  col_hashes = task$col_hashes
  checkmate::expect_character(col_hashes, any.missing = FALSE)
  checkmate::expect_names(names(col_hashes), permutation.of = setdiff(unlist(task$col_roles), task$backend$primary_key))

  # if multiple cols have the same hash, check that they actually contain the same data.
  if (!null_backend) {
    realhashes = sapply(task$data(cols = names(col_hashes)), mlr3misc::calculate_hash)
    expect_equal(unname(sapply(split(realhashes, col_hashes), data.table::uniqueN)), rep(1, data.table::uniqueN(col_hashes)))
  }

}

expect_task_supervised = function(task) {
  checkmate::expect_r6(task, "TaskSupervised", cloneable = TRUE)
  checkmate::expect_subset(task$target_names, task$col_info$id, empty.ok = FALSE)

  f = task$formula()
  checkmate::expect_class(f, "formula")
  # tf = terms(f)
  # checkmate::expect_set_equal(labels(tf), task$feature_names) # rhs
  # checkmate::expect_set_equal(setdiff(all.vars(tf), labels(tf)), task$target_names) # lhs
  checkmate::expect_subset(task$feature_names, colnames(head(task)))
  expect_hash(task$hash, 1L)
}

expect_task_classif = function(task) {
  checkmate::expect_r6(task, "TaskClassif")
  y = task$truth()
  checkmate::expect_factor(y)

  testthat::expect_gte(length(task$class_names), 2L)
  checkmate::expect_character(task$class_names, any.missing = FALSE)
  checkmate::expect_subset(task$class_names, levels(y))
  if (length(task$class_names) > 2L) {
    testthat::expect_identical(task$positive, NA_character_)
    testthat::expect_identical(task$negative, NA_character_)
  } else {
    checkmate::expect_set_equal(c(task$positive, task$negative), task$class_names)
    testthat::expect_true(task$positive != task$negative)
  }
  expect_hash(task$hash, 1L)
}

is_special_learner = function(x) {
  inherits(x, "GraphLearner") || inherits(x, "AutoTuner") || inherits(x, "AutoFSelector")
}

expect_task_regr = function(task) {
  checkmate::expect_r6(task, "TaskRegr")
  y = task$truth()
  checkmate::expect_numeric(y, any.missing = FALSE)
  expect_hash(task$hash, 1L)
}

expect_task_unsupervised = function(task) {
  checkmate::expect_r6(task, "TaskUnsupervised")
  expect_hash(task$hash, 1L)
}

expect_task_generator = function(gen) {
  checkmate::expect_r6(gen, "TaskGenerator", private = ".generate")
  expect_id(gen$id)
  checkmate::expect_string(gen$label, na.ok = TRUE)
  expect_man_exists(gen$man)
  checkmate::expect_choice(gen$task_type, mlr3::mlr_reflections$task_types$type)
  checkmate::expect_function(gen$generate, args = "n")
  checkmate::expect_class(gen$param_set, "ParamSet")
  checkmate::expect_list(gen$param_set$values, names = "unique")
  testthat::expect_output(print(gen))
}


expect_learner = function(lrn, task = NULL, check_man = TRUE) {

  checkmate::expect_r6(lrn, "Learner", cloneable = TRUE)
  expect_id(lrn$id)
  checkmate::expect_string(lrn$label, na.ok = TRUE)
  if (check_man) {
    expect_man_exists(lrn$man)
  }
  testthat::expect_output(print(lrn))

  checkmate::expect_choice(lrn$task_type, mlr3::mlr_reflections$task_types$type)
  checkmate::expect_character(lrn$packages, any.missing = FALSE, min.chars = 1L, unique = TRUE)
  checkmate::expect_class(lrn$param_set, "ParamSet")
  testthat::expect_lte(length(lrn$param_set$ids(tags = "threads")), 1L)
  checkmate::expect_character(lrn$properties, any.missing = FALSE, min.chars = 1L, unique = TRUE)

  checkmate::expect_character(lrn$predict_types, any.missing = FALSE, min.chars = 1L, unique = TRUE)
  checkmate::expect_choice(lrn$predict_type, lrn$predict_types)
  checkmate::expect_subset(lrn$feature_types, mlr3::mlr_reflections$task_feature_types, empty.ok = FALSE)

  expect_hash(lrn$hash)
  expect_hash(lrn$phash)

  checkmate::expect_function(mlr3misc::get_private(lrn)$.train, args = "task", nargs = 1L)
  checkmate::expect_function(mlr3misc::get_private(lrn)$.predict, args = "task", nargs = 1L)
  expect_hash(lrn$hash, 1L)

  tags = lrn$param_set$tags
  tags = Filter(function(tags) !any(c("train", "predict") %in% tags), tags)
  testthat::expect_true(length(tags) == 0L,
    info = sprintf("All hyperparameters of learner %s must be tagged with 'train' or 'predict'. Missing tags for: %s", lrn$id, paste0(names(tags), collapse = ", "))
  )

  # FIXME: remove at and glrn when they have new releases supporting marshaling
  if ("marshal" %in% lrn$properties && !inherits(lrn, "GraphLearner") && !inherits(lrn, "AutoTuner") && !inherits(lrn, "AutoFSelector")) {
    checkmate::expect_function(lrn$marshal)
    checkmate::expect_function(lrn$unmarshal)
  }

  if (!is.null(task)) {
    checkmate::expect_class(task, "Task")
    checkmate::expect_subset(lrn$properties, mlr3::mlr_reflections$learner_properties[[task$task_type]])
    testthat::expect_identical(lrn$task_type, task$task_type)

    # FIXME: remove at and glrn when they have new releases supporting marshaling
    if ("marshal" %in% lrn$properties && !inherits(lrn, "GraphLearner") && !inherits(lrn, "AutoTuner") && !inherits(lrn, "AutoFSelector")) {
      expect_marshalable_learner(lrn, task)
    }
  }

  if (!inherits(lrn, "GraphLearner") && !inherits(lrn, "AutoTuner")) { # still not in pipelines, breaking check in mlr3tuning
    checkmate::expect_class(lrn$base_learner(), "Learner")
  }

  if ("validation" %in% lrn$properties && !test_class(lrn, "GraphLearner")) {
    testthat::expect_true(exists("validate", lrn))
    testthat::expect_true(exists("internal_valid_scores", envir = lrn))
    checkmate::expect_function(mlr3misc::get_private(lrn)$.extract_internal_valid_scores)
  } else if (!is_special_learner(lrn)){
    checkmate::assert_false(exists("validate", lrn))
  }
  if ("internal_tuning" %in% lrn$properties && !test_class(lrn, "GraphLearner")) {
    any_internal_tuning = FALSE
    for (tags in lrn$param_set$tags) {
      if ("internal_tuning" %in% tags) {
        any_internal_tuning = TRUE
        break
      }
    }
    if (!any_internal_tuning) {
      stopf("at least one parameter must support internal tuning when the learner is tagged as such")
    }
    testthat::expect_true(exists("internal_tuned_values", envir = lrn))
    checkmate::expect_function(mlr3misc::get_private(lrn)$.extract_internal_tuned_values)
  }
}

expect_marshalable_learner = function(learner, task) {
  testthat::expect_true("marshal" %in% learner$properties)
  learner$state = NULL

  has_public = function(learner, x) {
    exists(x, learner, inherits = FALSE)
  }

  testthat::expect_true(has_public(learner, "marshal") && checkmate::test_function(learner$marshal, nargs = 0))
  testthat::expect_true(has_public(learner, "unmarshal") && checkmate::test_function(learner$unmarshal, nargs = 0))
  testthat::expect_true(has_public(learner, "marshaled"))

  testthat::expect_equal(learner$marshaled, FALSE)

  learner$train(task)
  model = learner$model
  class_prev = class(model)
  testthat::expect_false(learner$marshaled)
  testthat::expect_equal(mlr3::is_marshaled_model(learner$model), learner$marshaled)
  testthat::expect_invisible(learner$marshal())
  if (!inherits(learner, "GraphLearner")) {
    testthat::expect_true(learner$marshaled)
  }
  testthat::expect_equal(mlr3::is_marshaled_model(learner$model), learner$marshaled)

  # unmarshaling works
  testthat::expect_invisible(learner$unmarshal())
  # can predict after unmarshaling
  expect_prediction(learner$predict(task))
  # model is reset
  testthat::expect_equal(learner$model, model)
  # marshaled is set accordingly
  testthat::expect_false(learner$marshaled)
  testthat::expect_equal(class(learner$model), class_prev)
}

expect_resampling = function(r, task = NULL, strata = TRUE) {
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
    if (r$id %in% c("custom", "custom_cv", "loo")) {
      checkmate::expect_count(r$iters, na.ok = TRUE)
      testthat::expect_true(is.na(r$iters))
    }
    testthat::expect_identical(r$task_hash, NA_character_)
  } else {
    testthat::expect_true(r$is_instantiated)
    expect_hash(r$hash, 1L)
    expect_hash(r$task_hash, 1L)
    expect_hash(r$task_row_hash, 1L)
    if (!is.null(task)) {
      ids = task$row_ids
    }
    checkmate::expect_count(r$iters, positive = TRUE)

    for (i in seq_len(r$iters)) {
      train = r$train_set(1L)
      test = r$test_set(1L)
      checkmate::expect_integerish(train, any.missing = FALSE)
      checkmate::expect_integerish(test, any.missing = FALSE)
      if (!inherits(r, "ResamplingCustom") && !inherits(r, "ResamplingInsample")) {
        testthat::expect_length(intersect(train, test), 0L)
      }
      if (!is.null(task)) {
        checkmate::expect_subset(train, ids)
        checkmate::expect_subset(test, ids)
      }
    }
  }

  checkmate::expect_list(r$param_set$values, names = "unique")
  testthat::expect_true(checkmate::qtestr(r$param_set$values, "V1"))

  # check re-instantiation with provided task
  if (!is.null(task) && !is.null(task$backend) && !inherits(r, "ResamplingCustom") && !inherits(r, "ResamplingCustomCV")) {
    r = r$clone()$instantiate(task)
    checkmate::expect_subset(r$train_set(1), task$row_ids)
    checkmate::expect_subset(r$test_set(1), task$row_ids)

    # again with strata
    if (strata) {
      task = task$clone()
      task$col_roles$stratum = task$target_names
      r$instantiate(task)
      checkmate::expect_subset(r$train_set(1), task$row_ids)
      checkmate::expect_subset(r$test_set(1), task$row_ids)
    }
  }
}

expect_measure = function(m) {
  checkmate::expect_r6(m, "Measure", public = c("aggregate", "score", "id", "minimize", "packages", "range", "task_type", "task_properties"))
  expect_id(m$id)
  expect_man_exists(m$man)
  testthat::expect_output(print(m), "Measure")

  if ("requires_no_prediction" %in% m$properties) {
    testthat::expect_null(m$predict_sets)
  }

  expect_id(m$id)
  checkmate::expect_subset(m$task_type, c(NA_character_, mlr3::mlr_reflections$task_types$type), empty.ok = FALSE)
  checkmate::expect_numeric(m$range, len = 2, any.missing = FALSE)
  testthat::expect_lt(m$range[1], m$range[2])
  checkmate::expect_flag(m$minimize, na.ok = TRUE)
  checkmate::expect_character(m$packages, min.chars = 1L, any.missing = FALSE, unique = TRUE)
  if (is.null(mlr3misc::get_private(m)$.score)) {
    checkmate::expect_function(m$score_internal, args = c("prediction", "..."))
  } else {
    checkmate::expect_function(mlr3misc::get_private(m)$.score, args = c("prediction", "..."))
  }
  checkmate::expect_function(m$aggregate, args = "rr")
}

expect_prediction = function(p) {
  checkmate::expect_r6(p, "Prediction", public = c("row_ids", "truth", "predict_types"))
  testthat::expect_output(print(p), "Prediction")
  checkmate::expect_data_table(data.table::as.data.table(p), nrows  = length(p$row_ids))
  checkmate::expect_integerish(p$missing)
}

expect_prediction_regr = function(p) {
  expect_prediction(p)
  checkmate::expect_r6(p, "PredictionRegr", public = c("row_ids", "response", "truth", "predict_types", "se", "distr"))
  checkmate::expect_numeric(p$truth, any.missing = TRUE, len = length(p$row_ids), null.ok = TRUE)
  checkmate::expect_numeric(p$response, any.missing = FALSE, len = length(p$row_ids), null.ok = TRUE)
  if ("se" %in% p$predict_types) {
    checkmate::expect_numeric(p$se, any.missing = FALSE, len = length(p$row_ids), lower = 0)
  }
  if ("distr" %in% p$predict_types) {
    checkmate::expect_class(p$distr, "VectorDistribution")
  }
}

expect_prediction_classif = function(p, task = NULL) {
  expect_prediction(p)
  checkmate::expect_r6(p, "PredictionClassif", public = c("row_ids", "response", "truth", "predict_types", "prob"))
  n = length(p$row_ids)
  lvls = if (is.null(task)) levels(p$truth) else task$class_names
  checkmate::expect_factor(p$truth, len = n, levels = lvls, null.ok = TRUE)
  checkmate::expect_factor(p$response, len = n, levels = lvls, null.ok = TRUE)
  testthat::expect_identical(levels(p$truth), lvls)
  testthat::expect_identical(levels(p$response), lvls)
  if ("prob" %in% p$predict_types) {
    checkmate::expect_matrix(p$prob, "numeric", any.missing = FALSE, ncols = nlevels(p$response), nrows = n)
    testthat::expect_identical(colnames(p$prob), lvls)
  }
  confusion = p$confusion
  checkmate::expect_matrix(confusion)
  checkmate::expect_integer(confusion, lower = 0L, any.missing = FALSE)
  if (!is.null(task)) {
    checkmate::expect_names(rownames(confusion), identical.to = task$class_names)
    checkmate::expect_names(colnames(confusion), identical.to = task$class_names)
  }
}

expect_resample_result = function(rr, allow_incomplete = FALSE) {
  checkmate::expect_r6(rr, "ResampleResult")
  expect_resultdata(mlr3misc::get_private(rr)$.data, FALSE)
  testthat::expect_output(print(rr), "ResampleResult")
  nr = mlr3misc::get_private(rr)$.data$iterations()

  if (nr > 0L) {
    expect_task(rr$task, null_backend_ok = is.null(rr$task$backend))
    expect_learner(rr$learner)
    lapply(rr$learners, expect_learner, task = rr$task)
    expect_resampling(rr$resampling, task = rr$task)
  }

  expected_iters = if (allow_incomplete || nr == 0L) nr else rr$resampling$iters

  checkmate::expect_data_table(rr$score(), nrows = expected_iters, min.cols = length(mlr3::mlr_reflections$rr_names), any.missing = FALSE)
  checkmate::expect_names(names(rr$score()), must.include = mlr3::mlr_reflections$rr_names)
  if (nr > 0L) {
    expect_uhash(rr$uhash, 1L)
  } else {
    testthat::expect_identical(rr$uhash, NA_character_)
  }

  if (nr > 0L) {
    m = mlr3::default_measures(rr$task$task_type)[[1L]]
    y = rr$score(m)
    aggr = rr$aggregate(m)
    checkmate::expect_numeric(y[[m$id]], lower = m$range[1], upper = m$range[2], any.missing = FALSE, label = sprintf("measure %s", m$id))
    checkmate::expect_number(aggr[[m$id]], lower = m$range[1L], upper = m$range[2L], label = sprintf("measure %s", m$id))
  }

  checkmate::expect_list(rr$predictions(), types = "Prediction", len = expected_iters)
  if (nr > 0L) {
    expect_prediction(rr$prediction())
  }
}

expect_benchmark_result = function(bmr) {
  checkmate::expect_r6(bmr, "BenchmarkResult", private = ".data")
  expect_resultdata(mlr3misc::get_private(bmr)$.data, TRUE)
  testthat::expect_output(print(bmr), "BenchmarkResult")

  checkmate::expect_names(names(as.data.table(bmr)), permutation.of = c(mlr3::mlr_reflections$rr_names, "prediction", "uhash", "task_id", "learner_id", "resampling_id"))

  tab = bmr$tasks
  checkmate::expect_data_table(tab, ncols = 3L)
  checkmate::expect_names(names(tab), identical.to = c("task_hash", "task_id", "task"))
  expect_hash(tab$task_hash)
  expect_id(tab$task_id)
  checkmate::expect_list(tab$task, "Task")
  checkmate::expect_set_equal(bmr$tasks$task_hash, mlr3misc::get_private(bmr)$.data$data$tasks$task_hash)

  tab = bmr$learners
  checkmate::expect_data_table(tab, ncols = 3L)
  checkmate::expect_names(names(tab), identical.to = c("learner_hash", "learner_id", "learner"))
  expect_hash(tab$learner_hash)
  expect_id(tab$learner_id)
  checkmate::expect_list(tab$learner, "Learner")
  checkmate::expect_set_equal(bmr$learners$learner_hash, mlr3misc::get_private(bmr)$.data$data$learner_components$learner_hash)

  tab = bmr$resamplings
  checkmate::expect_data_table(tab, ncols = 3L)
  checkmate::expect_names(names(tab), identical.to = c("resampling_hash", "resampling_id", "resampling"))
  expect_hash(tab$resampling_hash)
  expect_id(tab$resampling_id)
  checkmate::expect_list(tab$resampling, "Resampling")
  checkmate::expect_set_equal(bmr$resamplings$resampling_hash, mlr3misc::get_private(bmr)$.data$data$resamplings$resampling_hash)

  if (nrow(mlr3misc::get_private(bmr)$.data$data$fact) > 0L) {
    measures = mlr3::default_measures(bmr$task_type)
  } else {
    measures = mlr3::msrs("time_both")
  }
  tab = bmr$aggregate(measures, ids = TRUE)
  checkmate::expect_data_table(tab, min.cols = 5L + length(measures))
  checkmate::expect_names(names(tab), must.include = c("nr", "resample_result", "task_id", "learner_id", "resampling_id", "iters", mlr3misc::map_chr(measures, "id")))
  testthat::expect_equal(tab$nr, seq_len(nrow(tab)))
  checkmate::expect_list(tab$resample_result, "ResampleResult")
  expect_id(tab$task_id)
  expect_id(tab$learner_id)
  expect_id(tab$resampling_id)
  if (length(measures)) {
    measure = measures[[1L]]
    checkmate::expect_numeric(tab[[measure$id]], any.missing = FALSE)
  }

  tab = bmr$aggregate(measures = measures, params = TRUE)
  checkmate::expect_list(tab$params)

  uhashes = bmr$uhashes
  expect_uhash(uhashes, len = length(mlr3misc::get_private(bmr)$.data$uhashes()))
  checkmate::expect_set_equal(uhashes, unique(mlr3misc::get_private(bmr)$.data$uhashes()))
  testthat::expect_equal(bmr$n_resample_results, length(uhashes))

  tab = bmr$resample_results
  checkmate::expect_data_table(tab, ncols = 3L, nrows = bmr$n_resample_results, any.missing = FALSE)
  checkmate::expect_character(tab$uhash, any.missing = FALSE)
  checkmate::expect_integer(tab$nr, sorted = TRUE, any.missing = FALSE, lower = 1L)
  expect_integer(tab$nr, any.missing = FALSE, lower = 1L)
  checkmate::expect_list(tab$resample_result, types = "ResampleResult")

  ni = mlr3misc::get_private(bmr)$.data$iterations()
  if (ni) {
    checkmate::expect_choice(bmr$task_type, mlr3::mlr_reflections$task_types$type, null.ok = ni == 0L)
  } else {
    testthat::expect_null(bmr$task_type)
  }
}

expect_resultdata = function(rdata, consistency = TRUE) {
  checkmate::expect_class(rdata, "ResultData")
  data = rdata$data

  proto = mlr3:::star_init()
  checkmate::expect_set_equal(names(data), names(proto))

  for (nn in names(proto)) {
    checkmate::expect_data_table(data[[nn]], key = data.table::key(proto[[nn]]))
    testthat::expect_equal(names(data[[nn]]), names(proto[[nn]]))
  }

  checkmate::expect_character(data$uhashes$uhash, unique = TRUE)

  expect_fsetequal = function(x, y, column) {
    testthat::expect_true(data.table::fsetequal(x[, column, with = FALSE], y[, column, with = FALSE], all = FALSE))
  }

  if (consistency) {
    expect_fsetequal(data$fact, data$uhashes, "uhash")
    expect_fsetequal(data$fact, data$tasks, "task_hash")
    expect_fsetequal(data$fact, data$learners, "learner_phash")
    expect_fsetequal(data$fact, data$learner_components, "learner_hash")
    expect_fsetequal(data$fact, data$resamplings, "resampling_hash")
  }
}

expect_no_extra_pkgs = function(expr, pkgs = character()) {
  req_pkgs = function(expr, pkgs) {
    library("mlr3")
    for (pkg in pkgs) {
      requireNamespace(pkg, quietly = TRUE)
    }
    snap = loadedNamespaces()
    eval(expr)
    setdiff(loadedNamespaces(), snap)
  }

  skip_if_not_installed("callr")
  extra = callr::r(req_pkgs, args = list(substitute(expr), pkgs = pkgs))
  expect_identical(extra, character(), info = "extra packages required for construction")
}

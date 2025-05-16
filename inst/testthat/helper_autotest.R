#' @title Learner Autotest Suite
#'
#' @description
#' The autotest suite is a collection of functions to test learners in a standardized way.
#' Extension packages need to specialize the S3 methods in the file.
#
#' @details
#' `run_autotest(learner)` generates multiple tasks, depending on the properties of the learner and tests the learner on each task, with each predict type.
#' Calls `generate_tasks()` to generate tasks and `run_experiment()` to run the experiments.
#' See `generate_tasks()` for a list of tasks that are generated.
#' To debug, simply run `result = run_autotest(learner)` and proceed with investigating he task, learner and prediction of the returned `result`.
#'
#' `run_experiment(task, learner)` runs a single experiment.
#' Calls `train()` and `predict()` on the learner and checks the prediction with `score()`.
#' The prediction is checked with `sanity_check()`.
#'
#' `generate_tasks(learner)` generates multiple tasks for a given learner.
#' Calls `generate_data()` and `generate_generic_tasks()` to generate tasks with different feature types.
#'
#' @noRd
NULL

#' @title Generate Tasks for a Learner
#'
#' @description
#' Generates multiple tasks for a given [Learner], based on its properties.
#'
#' @param learner [Learner]\cr
#'  Learner to generate tasks for.
#' @param proto [Task]\cr
#'  Prototype task to generate tasks from.
#'
#' @return (List of [Task]s).
#'
#' @noRd
generate_generic_tasks = function(learner, proto) {
  tasks = list()
  n = proto$nrow
  p = length(proto$feature_names)

  if (p > 0L) {
    # individual tasks with each supported feature type
    for (ftype in learner$feature_types) {
      sel = proto$feature_types[ftype, "id", on = "type", with = FALSE][[1L]]
      tasks[[sprintf("feat_single_%s", ftype)]] = proto$clone(deep = TRUE)$select(sel)
    }
  }

  # task with all supported features types
  sel = proto$feature_types[list(learner$feature_types), "id", on = "type", with = FALSE, nomatch = NULL][[1L]]
  tasks$feat_all = proto$clone(deep = TRUE)$select(sel)

  # task with missing values
  if ("missings" %in% learner$properties) {
    # one missing val in each feature
    features = proto$feature_names
    rows = sample(n, length(features))
    data = proto$data(cols = features)
    for (j in seq_along(features)) {
      data.table::set(data, rows[j], features[j], NA)
    }
    tasks$missings = proto$clone(deep = TRUE)$select(character())$cbind(data)

    if (length(features)) {
      # no row with no missing -> complete.cases() won't help
      features = sample(features, n, replace = TRUE)
      data = proto$data(cols = proto$feature_names)
      for (i in seq_along(features))
        data.table::set(data, i = i, j = features[i], NA)
      tasks$missings_each_row = proto$clone(deep = TRUE)$select(character())$cbind(data)
    }
  }

  # task with weights
  if ("weights" %in% learner$properties) {
    tmp = proto$clone(deep = TRUE)$cbind(data.frame(weights = runif(n)))
    tmp$set_col_roles(cols = "weights", roles = "weights_learner")
    tasks$weights_learner = tmp
  }

  # task with offset
  if ("offset" %in% learner$properties) {
    if ("multiclass" %in% learner$properties && "multiclass" %in% proto$properties) {
      offset_cols = paste0("offset_", proto$class_names)
      # One offset column per class
      offset_data = as.data.frame(
        mlr3misc::set_names(
          lapply(offset_cols, function(col) runif(n)),
          offset_cols
        )
      )
      tmp = proto$clone(deep = TRUE)$cbind(offset_data)
      tmp$set_col_roles(cols = offset_cols, roles = "offset")
    } else {
      tmp = proto$clone(deep = TRUE)$cbind(data.frame(offset = runif(n)))
      tmp$set_col_roles(cols = "offset", roles = "offset")
    }
    tasks$offset = tmp
  }

  # task with non-ascii feature names
  if (p > 0L) {
    sel = proto$feature_types[list(learner$feature_types), "id", on = "type", with = FALSE, nomatch = NULL][[1L]]
    tasks$utf8_feature_names = proto$clone(deep = TRUE)$select(sel)
    old = sel[1L]
    new = "\u00e4 + \u1e9e"
    tasks$utf8_feature_names$rename(old, new)
  }

  # make sure that task ids match list names
  mlr3misc::imap(tasks, function(x, n) {
    x$id = n
    x
  })
}

#' @title Generate Data for a Learner
#'
#' @description
#' Generates data for a given [Learner], based on its supported feature types.
#' Data is created for logical, integer, numeric, character, factor, ordered, and POSIXct features.
#'
#' @param learner [Learner]\cr
#'  Learner to generate data for.
#' @param N `integer(1)`\cr
#'  Number of rows of generated data.
#'
#' @return [data.table::data.table()]
#'
#' @noRd
generate_data = function(learner, N) {
  generate_feature = function(type) {
    switch(type,
      logical = sample(rep_len(c(TRUE, FALSE), N)),
      integer = sample(rep_len(1:3, N)),
      numeric = runif(N),
      character = sample(rep_len(letters[1:2], N)),
      factor = sample(factor(rep_len(c("f1", "f2"), N), levels = c("f1", "f2"))),
      ordered = sample(ordered(rep_len(c("o1", "o2"), N), levels = c("o1", "o2"))),
      POSIXct = Sys.time() - runif(N, min = 0, max = 10 * 365 * 24 * 60 * 60),
      Date = Sys.Date() - runif(N, min = 0, max = 10 * 365)
    )
  }
  types = unique(learner$feature_types)
  do.call(data.table::data.table, mlr3misc::set_names(mlr3misc::map(types, generate_feature), types))
}

#' @title Generate Tasks for a Learner
#'
#' @description
#' Generates multiple tasks for a given [Learner], based on its properties.
#' This function is primarily used for unit tests, but can also assist while writing custom learners.
#' The following tasks are created:
#' * `feat_single_*`: Tasks with a single feature type.
#' * `feat_all_*`: Task with all supported feature types.
#' * `missings_*`: Task with missing values.
#' * `utf8_feature_names_*`: Task with non-ascii feature names.
#' * `sanity`: Task with a simple dataset to check if the learner is working.
#' * `sanity_reordered`: Task with the same dataset as `sanity`, but with reordered columns.
#' * `sanity_switched`: Task with the same dataset as `sanity`, but with the positive class switched.
#'
#' @param learner [Learner]\cr
#'  Learner to generate tasks for.
#' @param N `integer(1)`\cr
#'   Number of rows of generated tasks.
#'
#' @return `list` of [Task]s
#' @keywords internal
#' @export
#' @examples
#' tasks = generate_tasks(lrn("classif.rpart"))
#' tasks$missings_binary$data()
generate_tasks = function(learner, N = 30L) {
  N = checkmate::assert_int(N, lower = 10L, coerce = TRUE)
  UseMethod("generate_tasks")
}

#' @export
generate_tasks.LearnerClassif = function(learner, N = 30L) {
  tasks = list()

  # generate binary tasks
  if ("twoclass" %in% learner$properties) {
    target = factor(rep_len(head(LETTERS, 2L), N))
    data = cbind(data.table::data.table(target = target), generate_data(learner, N))
    task = mlr3::TaskClassif$new("proto", mlr3::as_data_backend(data), target = "target", positive = "A")
    gen_tasks = generate_generic_tasks(learner, task)
    # set names
    lapply(gen_tasks, function(x) x$id = paste0(x$id, "_binary"))
    gen_tasks = mlr3misc::set_names(gen_tasks, paste0(names(gen_tasks), "_binary"))
    tasks = c(tasks, gen_tasks)
  }

  # generate multiclass tasks
  if ("multiclass" %in% learner$properties) {
    target = factor(rep_len(head(LETTERS, 3L), N))
    data = cbind(data.table::data.table(target = target), generate_data(learner, N))
    task = mlr3::TaskClassif$new("proto", mlr3::as_data_backend(data), target = "target")
    gen_tasks = generate_generic_tasks(learner, task)
    # set names
    lapply(gen_tasks, function(x) x$id = paste0(x$id, "_multiclass"))
    gen_tasks = mlr3misc::set_names(gen_tasks, paste0(names(gen_tasks), "_multiclass"))
    tasks = c(tasks, gen_tasks)
  }

  # generate sanity task
  data = with_seed(100, {
    data = data.table::data.table(x = c(rnorm(100, 0, 1), rnorm(100, 10, 1)), y = rep(as.factor(c("A", "B")), each = 100))
    data$unimportant = runif(nrow(data), min = 0, max = 3)
    data
  })
  tasks$sanity = mlr3::TaskClassif$new("sanity", mlr3::as_data_backend(data), target = "y", positive = "A")

  # sanity task, but reorder columns between train and predict in run_experiment()
  tasks$sanity_reordered = mlr3::TaskClassif$new("sanity_reordered", mlr3::as_data_backend(data), target = "y")

  # sanity task, but with other label as positive class to detect label switches
  tasks$sanity_switched = mlr3::TaskClassif$new("sanity_switched", mlr3::as_data_backend(data), target = "y", positive = "B")

  tasks
}
registerS3method("generate_tasks", "LearnerClassif", generate_tasks.LearnerClassif)

#' @export
generate_tasks.LearnerRegr = function(learner, N = 30L) {
  target = rnorm(N)
  data = cbind(data.table::data.table(target = target), generate_data(learner, N))
  task = mlr3::TaskRegr$new("proto", mlr3::as_data_backend(data), target = "target")

  tasks = generate_generic_tasks(learner, task)

  # generate sanity task
  data = with_seed(100, {
    y = seq(from = -10, to = 10, length.out = 100)
    data.table::data.table(
      y = y,
      x = y + rnorm(length(y), mean = 1),
      unimportant = runif(length(y), min = 0, max = 1)
    )
  })
  tasks$sanity = mlr3::TaskRegr$new("sanity", mlr3::as_data_backend(data), target = "y")
  tasks$sanity_reordered = mlr3::TaskRegr$new("sanity_reordered", mlr3::as_data_backend(data), target = "y")

  tasks
}
registerS3method("generate_tasks", "LearnerRegr", generate_tasks.LearnerRegr)

#' @title Sanity Check for Predictions
#'
#' @description
#' Checks the sanity of a prediction.
#'
#' @param prediction [Prediction]\cr
#'  Prediction to check.
#'
#' @return (`logical(1)`).
#'
#' @noRd
sanity_check = function(prediction, ...) {
  UseMethod("sanity_check")
}

sanity_check.PredictionClassif = function(prediction, ...) {
  prediction$score(mlr3::msr("classif.ce")) <= 0.3
}
registerS3method("sanity_check", "LearnerClassif", sanity_check.PredictionClassif)


sanity_check.PredictionRegr = function(prediction, ...) {
  prediction$score(mlr3::msr("regr.mse")) <= 2
}
registerS3method("sanity_check", "LearnerRegr", sanity_check.PredictionRegr)


#' @title Run a Single Learner Test
#'
#' @description
#' Runs a single experiment with a given task and learner.
#'
#' @param task [Task]\cr
#'  Task to run the experiment on.
#' @param learner [Learner]\cr
#'  Learner to run the experiment with.
#' @param seed `integer(1)`\cr
#'  Seed to use for the experiment.
#'  If `NULL`, a random seed is generated.
#' @param configure_learner `function(learner, task)`\cr
#'  Function to configure the learner before training.
#'  Useful when learner settings need to be adjusted for a specific task.
#'
#' @return `list` with the following elements:
#'  - `ok` (`logical(1)`): Success flag.
#'  - `learner` ([Learner]): Learner used for the experiment.
#'  - `prediction` ([Prediction]): Prediction object.
#'  - `error` (`character()`): Error message if `ok` is `FALSE`.
#'  - `seed` (`integer(1)`): Seed used for the experiment.
#'
#' @noRd
run_experiment = function(task, learner, seed = NULL, configure_learner = NULL) {

  # function to collect error message and objects
  err = function(info, ...) {
    info = sprintf(info, ...)
    list(
      ok = FALSE, seed = seed,
      task = task, learner = learner, prediction = prediction, score = score,
      error = sprintf("[%s] learner '%s' on task '%s' failed: %s",
        stage, learner$id, task$id, info)
    )
  }

  # seed handling
  if (is.null(seed)) {
    seed = sample.int(floor(.Machine$integer.max / 2L), 1L)
  }

  old_seed = get0(".Random.seed", globalenv(), mode = "integer", inherits = FALSE)
  if (is.null(old_seed)) {
    runif(1L)
    old_seed = get0(".Random.seed", globalenv(), mode = "integer", inherits = FALSE)
  }
  on.exit(assign(".Random.seed", old_seed, globalenv()), add = TRUE)

  set.seed(seed)

  task = mlr3::assert_task(mlr3::as_task(task))
  learner = mlr3::assert_learner(mlr3::as_learner(learner, clone = TRUE))
  if (!is.null(configure_learner)) {
    configure_learner(learner = learner, task = task)
  }
  prediction = NULL
  score = NULL

  # check train
  stage = "train()"

  # enable weights
  # the next lines are maybe not strictly necessary, but test that the defaults are
  # what they should be
  if ("weights" %in% learner$properties) {
    if (learner$use_weights != "use") {
      return(err("use_weights != 'use' for learner with property 'weights' on init!"))
    }
  } else {
    if (learner$use_weights != "error") {
      return(err("use_weights != 'error' for learner without property 'weights' on init!"))
    }
  }

  ok = suppressWarnings(try(learner$train(task), silent = TRUE))
  if (inherits(ok, "try-error")) {
    return(err(as.character(ok)))
  }
  if (is.null(learner$model)) {
    return(err("model is NULL"))
  }

  # check predict
  stage = "predict()"

  prediction = suppressWarnings(try(learner$predict(task), silent = TRUE))
  if (inherits(prediction, "try-error")) {
    ok = prediction
    prediction = NULL
    return(err(as.character(ok)))
  }
  msg = checkmate::check_class(prediction, "Prediction")
  if (!isTRUE(msg)) {
    return(err(msg))
  }
  if (prediction$task_type != learner$task_type) {
    return(err("learner and prediction have different task_type"))
  }

  # catch for mlr3proba tasks, which all return every possible predict type
  if (!(learner$task_type %in% c("dens", "surv"))) {
    expected = mlr3::mlr_reflections$learner_predict_types[[learner$task_type]][[learner$predict_type]]
    msg = checkmate::check_subset(expected, prediction$predict_types, empty.ok = FALSE)
    if (!isTRUE(msg)) {
      return(err(msg))
    }

    if (learner$predict_type == "response") {
      msg = checkmate::check_set_equal(learner$predict_type, prediction$predict_types)
      if (!isTRUE(msg)) {
        return(err(msg))
      }
    } else {
      msg = checkmate::check_subset(learner$predict_type, prediction$predict_types, empty.ok = FALSE)
      if (!isTRUE(msg)) {
        return(err(msg))
      }
    }
  }

  if (grepl("reordered", task$id)) {
    # compare prediction with reordered newdata
    newdata = task$data(cols = rev(task$feature_names))
    tmp = learner$predict_newdata(newdata)

    if (!isTRUE(all.equal(prediction$response, tmp$response))) {
      return(err("Task columns cannot be reordered"))
    }
  }

  # check score
  stage = "score()"

  score = try(
    prediction$score(mlr3::default_measures(learner$task_type),
      task = task,
      learner = learner,
      train_set = task$row_ids
  ), silent = TRUE)
  if (inherits(score, "try-error")) {
    ok = score
    score = NULL
    return(err(as.character(ok)))
  }
  msg = checkmate::check_numeric(score, any.missing = FALSE)
  if (!isTRUE(msg)) {
    return(err(msg))
  }

  # run sanity check on sanity task
  if (startsWith(task$id, "sanity") && !sanity_check(prediction, task = task, learner = learner, train_set = task$row_ids)) {
    return(err("sanity check failed"))
  }

  # check importance, selected_features and oob_error methods
  if (startsWith(task$id, "feat_all")) {
    if ("importance" %in% learner$properties) {
      importance = learner$importance()
      msg = checkmate::check_numeric(rev(importance), any.missing = FALSE, min.len = 1L, sorted = TRUE)
      if (!isTRUE(msg)) {
        return(err(msg))
      }
      msg = checkmate::check_names(names(importance), subset.of = task$feature_names)
      if (!isTRUE(msg)) {
        return(err("Names of returned importance scores do not match task names: %s", str_collapse(names(importance))))
      }
      if ("unimportant" %in% head(names(importance), 1L)) {
        return(err("unimportant feature is important"))
      }
    }

    if ("selected_features" %in% learner$properties) {
      selected = learner$selected_features()
      msg = checkmate::check_subset(selected, task$feature_names)
      if (!isTRUE(msg)) {
        return(err(msg))
      }
    }

    if ("oob_error" %in% learner$properties) {
      oob = learner$oob_error()
      msg = checkmate::check_number(oob)
      if (!isTRUE(msg)) {
        return(err(msg))
      }
    }
  }

  if ("weights" %in% learner$properties) {
    use_weights = learner$use_weights
    get_weights = learner$.__enclos_env__$private$.get_weights
    counter = R6::R6Class("counter", public = list(count = 0, count_up = function(x) self$count = self$count + 1))$new()
    get_weights_counter = function(...) {
      counter$count_up()
      get_weights(...)
    }
    on.exit({
      learner$.__enclos_env__$private$.get_weights = get_weights
      lockBinding(".get_weights", env = learner$.__enclos_env__$private)
      learner$use_weights = use_weights
    })

    unlockBinding(".get_weights", env = learner$.__enclos_env__$private)
    learner$.__enclos_env__$private$.get_weights = get_weights_counter

    learner$use_weights = "use"
    suppressWarnings(learner$train(task))
    if (counter$count == 0) {
      return(err("get_weights was not called"))
    }

    learner$use_weights = "ignore"
    suppressWarnings(learner$train(task))
    if (counter$count == 0) {
      return(err("get_weights was not called. It should be called even when use_weights = 'ignore'"))
    }

    learner$.__enclos_env__$private$.get_weights = get_weights
    lockBinding(".get_weights", env = learner$.__enclos_env__$private)
    learner$use_weights = use_weights
    on.exit()
  }

  return(list(ok = TRUE, learner = learner, prediction = prediction, error = character(), seed = seed))
}

#' @title Run Autotest for a Learner
#'
#' @description
#' Runs a series of experiments with a given learner on multiple tasks.
#'
#' @param learner ([Learner])\cr
#'  The learner to test.
#' @param N (`integer(1)`)\cr
#'  Number of rows of generated tasks.
#' @param exclude (`character()`)\cr
#'  Regular expression to exclude tasks from the test.
#'  Run `generate_tasks(learner)` to see all available tasks.
#' @param predict_types (`character()`)\cr
#'  Predict types to test.
#' @param check_replicable (`logical(1)`)\cr
#'  Check if the results are replicable.
#' @param configure_learner (`function(learner, task)`)\cr
#'  Function to configure the learner before training.
#'  Useful when learner settings need to be adjusted for a specific task.
#'
#' @return If the test was successful, `TRUE` is returned.
#' If the test failed, a `list` with the following elements is returned:
#'  - `ok` (`logical(1)`): Success flag.
#'  - `seed` (`integer(1)`): Seed used for the experiment.
#'  - `task` ([Task]): Task used for the experiment.
#'  - `learner` ([Learner]): Learner used for the experiment.
#'  - `prediction` ([Prediction]): Prediction object.
#'  - `score` (`numeric(1)`): Score of the prediction.
#'  - `error` (`character()`): Error message if `ok` is `FALSE`.
#
#' @noRd
run_autotest = function(learner, N = 30L, exclude = NULL, predict_types = learner$predict_types, check_replicable = TRUE, configure_learner = NULL) { # nolint
  if (!is.null(configure_learner)) {
    checkmate::assert_function(configure_learner, args = c("learner", "task"))
  }
  learner = learner$clone(deep = TRUE)
  id = learner$id
  tasks = generate_tasks(learner, N = N)

  if (!is.null(exclude)) {
    tasks = tasks[!grepl(exclude, names(tasks))]
  }

  sanity_runs = list()
  make_err = function(msg, ...) {
    run$ok = FALSE
    run$error = sprintf(msg, ...)
    run
  }

  for (task in tasks) {
    for (predict_type in predict_types) {
      learner$id = sprintf("%s:%s", id, predict_type)
      learner$predict_type = predict_type

      if (predict_type == "quantiles") {
        learner$quantiles = 0.5
      }

      run = run_experiment(task, learner)
      if (!run$ok) {
        return(run)
      }

      # re-run task with same seed for feat_all
      if (startsWith(task$id, "feat_all")) {
        repeated_run = run_experiment(task, learner, seed = run$seed)

        if (!repeated_run$ok) {
          return(repeated_run)
        }

        if (check_replicable && !isTRUE(all.equal(as.data.table(run$prediction), as.data.table(repeated_run$prediction)))) {
          return(make_err("Different results for replicated runs using fixed seed %i", run$seed))
        }
      }

      if (task$task_type == "classif" && task$id == "sanity") {
        sanity_runs[[predict_type]] = run
      }
    }
    if (task$task_type == "classif" && length(sanity_runs) > 1L) {
      responses = lapply(sanity_runs, function(r) r$prediction$response)
      if (!isTRUE(Reduce(all.equal, responses))) {
        return(make_err("Response is different for different predict types"))
      }
    }
  }

  return(TRUE)
}

#' @title Check Parameters of mlr3 Learners
#'
#' @description
#' Checks parameters of mlr3learners against parameters defined in the upstream functions of the respective learners.
#'
#' @details
#' Some learners do not have all of their parameters stored within the learner function that is called within `.train()`.
#' Sometimes learners come with a "control" function, e.g. [glmnet::glmnet.control()].
#' Such need to be checked as well since they make up the full ParamSet of the respective learner.
#'
#' To work nicely with the defined ParamSet, certain parameters need to be excluded because these are only present in either the "control" object or the actual top-level function call.
#' Such exclusions should go into argument `exclude` with a comment for the reason of the exclusion.
#' See examples for more information.
#'
#' @param learner ([mlr3::Learner])\cr
#'   The constructed learner.
#' @param fun (`function`)\cr
#'   The function of the upstream package for which parameters should
#'   be checked. E.g. `mboost::boost_control`.
#' @exclude (`character()`)\cr
#'   Parameters which should be excluded for this particular check. See details.
#' @examples
#' test_that("classif.gamboost", {
#'   learner = lrn("classif.gamboost")
#'   fun = mboost::gamboost
#'   exclude = c(
#'     "formula", # .train
#'     "data", # .train
#'     "na.action", # Only na.omit and na.fail available
#'     "weights", # .train
#'     "control" # mboost::boost_control
#'   )
#'
#'   run_paramtest(learner, fun, exclude)
#'   expect_true(result, info = result$error)
#' })
run_paramtest = function(learner, fun, exclude = character(), tag = NULL) {
  par_learner = learner$param_set$ids(tags = tag)
  if (checkmate::test_list(fun)) {
    # for xgboost we pass a character vector with info scraped from the web
    if (mlr3misc::some(fun, function(x) class(x) == "character")) {
      which = which(mlr3misc::map_lgl(fun, function(x) class(x) == "character"))
      par_package = fun[[which]]
      fun[[which]] = NULL
      other = unlist(lapply(fun, formalArgs))
      par_package = append(par_package, other)
    } else {
      par_package = unlist(lapply(fun, formalArgs))
    }
  } else {
    par_package = formalArgs(fun)
  }

  missing = setdiff(par_package, par_learner)
  missing = setdiff(missing, c(exclude, "..."))

  extra = setdiff(par_learner, par_package)
  extra = setdiff(extra, c(exclude, "..."))

  if (length(c(missing, extra)) == 0L) {
    return(TRUE)
  }

  merror = eerror = character(0)

  if (length(missing) > 0) {
    merror = sprintf("Missing parameters for learner '%s': %s",
      learner$id, paste0(missing, collapse = ", "))
  }

  if (length(extra) > 0) {
    eerror = sprintf("Extra parameters for learner '%s': %s",
      learner$id, paste0(extra, collapse = ", "))
  }

  error = paste(merror, eerror, sep = "\n")

  list(ok = FALSE, error = error, missing = missing, extra = extra)
}

# Helper function to convert a vector of probabilities to a matrix
#
# sometimes useful in tests, e.g., mlr3learners.partykit::LearnerClassifMob
# uses this in its tests to set up its custom prediction function for a mob
# version of a logit model
prob_vector_to_matrix = function(p, levs) {
  stopifnot(is.numeric(p))
  y = matrix(c(1 - p, p), ncol = 2L, nrow = length(p))
  colnames(y) = levs
  y
}

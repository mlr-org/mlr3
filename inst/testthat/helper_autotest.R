# Learner autotest suite
#
# `run_experiment(task, learner)` runs a single experiment.
# Returns a list with success flag "status" (`logical(1)`),
# "experiment" (partially constructed experiment), and "error"
# (informative error message).
#
# `run_autotest(learner)` generates multiple tasks, depending on the properties of the learner.
# and tests the learner on each task, with each predict type.
# To debug, simply run `result = run_autotest(learner)` and proceed with investigating
# the task, learner and prediction of the returned `result`.
#
# NB: Extension packages need to specialize the S3 methods in the file.


generate_generic_tasks = function(learner, proto) {
  tasks = list()

  if (length(proto$feature_names) > 1L) {
    # individual tasks with each supported feature type
    for (type in learner$feature_types) {
      sel = proto$feature_types[type, "id", on = "type", with = FALSE][[1L]]
      tasks[[sprintf("feat_single_%s", type)]] = proto$clone()$select(sel)
    }
  }

  # task with all supported features types
  sel = proto$feature_types[list(learner$feature_types), "id", on = "type", with = FALSE][[1L]]
  tasks$feat_all = proto$clone()$select(sel)

  # task with missing values
  if ("missings" %in% learner$properties) {
    # one missing val in each feature
    features = proto$feature_names
    rows = sample(proto$nrow, length(features))
    data = proto$data(cols = features)
    for (j in seq_along(features))
      data.table::set(data, rows[j], features[j], NA)
    tasks$missings = proto$clone()$replace_features(data)

    # no row with no missing -> complete.cases() won't help
    features = sample(features, proto$nrow, replace = TRUE)
    data = proto$data(cols = proto$feature_names)
    for (i in seq_along(features))
      data.table::set(data, i = i, j = features[i], NA)
    tasks$missings_each_row = proto$clone()$replace_features(data)
  }

  # task with weights
  if ("weights" %in% learner$properties) {
    tasks$weights = proto$clone()$cbind(data.frame(weights = runif(proto$nrow)))$set_col_role("weights", "weights", exclusive = TRUE)
  }

  # make sure that task ids match list names
  mlr3misc::imap(tasks, function(x, n) { x$id = n; x })
}

generate_data = function(learner, N) {
  generate_feature = function(type) {
    switch(type,
      logical = sample(rep_len(c(TRUE, FALSE), N)),
      integer = sample(rep_len(1:3, N)),
      numeric = runif(N),
      character = sample(rep_len(letters[1:2], N)),
      factor = sample(factor(rep_len(c("f1", "f2"), N), levels = c("f1", "f2"))),
      ordered = sample(ordered(rep_len(c("o1", "o2"), N), levels = c("o1", "o2")))
    )
  }
  types = unique(learner$feature_types)
  do.call(data.table::data.table, mlr3misc::set_names(mlr3misc::map(types, generate_feature), types))
}

#' @title Generate Tasks for a Learner
#'
#' @description
#' Generates multiple tasks for a given [Learner], based on its properties.
#' This function is primarily used for unit tests, but can also assist while
#' writing custom learners.
#'
#' @param learner :: [Learner].
#' @param N :: `integer(1)`\cr
#'   Number of rows of generated tasks.
#'
#' @return (List of [Task]s).
#' @keywords internal
#' @export
#' @examples
#' tasks = generate_tasks(mlr_learners$get("classif.rpart"))
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
    #set names
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
    #set names
    lapply(gen_tasks, function(x) x$id = paste0(x$id, "_multiclass"))
    gen_tasks = mlr3misc::set_names(gen_tasks, paste0(names(gen_tasks), "_multiclass"))
    tasks = c(tasks, gen_tasks)
  }

  # generate sanity task
  with_seed(100, {
    data = data.table::data.table(x = c(rnorm(100, 0, 1), rnorm(100, 10, 1)), y = rep(as.factor(c("A", "B")), each = 100))
    data$unimportant = runif(nrow(data))
  })
  tasks$sanity = mlr3::TaskClassif$new("sanity", mlr3::as_data_backend(data), target = "y", positive = "A")

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
  with_seed(100, {
    data = data.table::data.table(x = c(rnorm(100, 0, 1), rnorm(100, 10, 1)), y = c(rep(0, 100), rep(1, 100)))
    data$unimportant = runif(nrow(data))
  })
  task = mlr3misc::set_names(list(mlr3::TaskRegr$new("sanity", mlr3::as_data_backend(data), target = "y")), "sanity")
  tasks = c(tasks, task)
}
registerS3method("generate_tasks", "LearnerRegr", generate_tasks.LearnerRegr)

sanity_check = function(prediction) {
  UseMethod("sanity_check")
}

sanity_check.PredictionClassif = function(prediction) {
  prediction$score("classif.ce") <= 0.3
}
registerS3method("sanity_check", "LearnerClassif", sanity_check.PredictionClassif)


sanity_check.PredictionRegr = function(prediction) {
  prediction$score("regr.mse" <= 1)
}
registerS3method("sanity_check", "LearnerRegr", sanity_check.PredictionRegr)

run_experiment = function(task, learner) {
  err = function(info, ...) {
    info = sprintf(info, ...)
    list(
      ok = FALSE,
      task = task, learner = learner, prediction = prediction,
      error = sprintf("[%s] learner '%s' on task '%s' failed: %s",
        stage, learner$id, task$id, info)
    )
  }

  mlr3::assert_task(task)
  learner = mlr3::assert_learner(learner, task = task, clone = TRUE)
  prediction = NULL
  ctrl = list(encapsulate_train = "evaluate", encapsulate_predict = "evaluate")

  stage = "train()"
  ok = try(learner$train(task, ctrl = ctrl), silent = TRUE)
  if (inherits(ok, "try-error"))
    return(err(as.character(ok)))
  log = learner$log[stage == "train"]
  if ("error" %in% log$class)
    return(err("train log has errors: %s", mlr3misc::str_collapse(log[stage == "error", msg])))
  if (is.null(learner$model))
    return(err("model is NULL"))

  stage = "predict()"
  prediction = try(learner$predict(task), silent = TRUE)
  if (inherits(ok, "try-error"))
    return(err(as.character(ok)))
  log = learner$log[stage == "predict"]
  if ("error" %in% log$class)
    return(err("predict log has errors: %s", mlr3misc::str_collapse(log[stage == "error", msg])))
  if (!inherits(prediction, "Prediction"))
    return(err("$prediction has wrong class"))
  if (prediction$task_type != learner$task_type)
    return(err("learner and prediction have different task_type"))
  if (!all(learner$predict_type %in% prediction$predict_types))
    return(err("prediction is missing predict_types"))

  stage = "score()"
  perf = try(prediction$score(mlr_reflections$default_measures[[learner$task_type]]), silent = TRUE)
  if (inherits(ok, "try-error"))
    return(err(as.character(ok)))
  if (!checkmate::test_numeric(perf, any.missing = FALSE))
    return(err("score is not a numeric value"))

  # run sanity check on sanity task
  if (grepl("^sanity", task$id) && !sanity_check(prediction)) {
    return(err("sanity check failed"))
  }

  if (grepl("^feat_all", task$id) && "importance" %in% learner$properties) {
    imp = learner$importance()
    if (!checkmate::test_numeric(imp, any.missing = FALSE, min.len = 1L))
      return(err("importance is not numeric"))
    if (!checkmate::test_names(names(imp), subset.of = task$feature_names))
      return(err("importance is not properly named"))
    if (is.unsorted(rev(imp)))
      return(err("importance is not sorted"))
    if ("unimportant" %in% head(names(imp), 1L))
      return(err("unimportant feature is important"))
  }

  return(list(ok = TRUE, learner = learner, prediction = prediction, error = character(0)))
}

run_autotest = function(learner, N = 30L, exclude = NULL, predict_types = learner$predict_types) {
  learner = learner$clone(deep = TRUE)
  id = learner$id
  tasks = generate_tasks(learner, N = N)
  if (!is.null(exclude))
    tasks = tasks[!grepl(exclude, names(tasks))]

  for (task in tasks) {
    for (predict_type in predict_types) {
      learner$id = sprintf("%s:%s", id, predict_type)
      learner$predict_type = predict_type
      run = run_experiment(task, learner)
      if (!run$ok)
        return(run)
    }
  }

  return(TRUE)
}

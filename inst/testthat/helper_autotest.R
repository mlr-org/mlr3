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
    for (ftype in learner$feature_types) {
      sel = proto$feature_types[ftype, "id", on = "type", with = FALSE][[1L]]
      tasks[[sprintf("feat_single_%s", ftype)]] = proto$clone()$select(sel)
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
    tasks$missings = proto$clone()$select(character())$cbind(data)

    # no row with no missing -> complete.cases() won't help
    features = sample(features, proto$nrow, replace = TRUE)
    data = proto$data(cols = proto$feature_names)
    for (i in seq_along(features))
      data.table::set(data, i = i, j = features[i], NA)
    tasks$missings_each_row = proto$clone()$select(character())$cbind(data)
  }

  # task with weights
  if ("weights" %in% learner$properties) {
    tmp = proto$clone()$cbind(data.frame(weights = runif(proto$nrow)))
    tmp$col_roles$weight = "weights"
    tmp$col_roles$features = setdiff(tmp$col_roles$features, "weights")
    tasks$weights = tmp
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
    x = seq(from = -10, to = 10, length.out = 100)
    data.table::data.table(
      y = rnorm(length(x), mean = 1),
      x = x,
      unimportant = runif(length(x), min = 0, max = 3)
    )
  })
  tasks$sanity = mlr3::TaskRegr$new("sanity", mlr3::as_data_backend(data), target = "y")
  tasks$sanity_reordered = mlr3::TaskRegr$new("sanity_reordered", mlr3::as_data_backend(data), target = "y")

  tasks
}
registerS3method("generate_tasks", "LearnerRegr", generate_tasks.LearnerRegr)

sanity_check = function(prediction) {
  UseMethod("sanity_check")
}

sanity_check.PredictionClassif = function(prediction) {
  prediction$score(mlr3::msr("classif.ce")) <= 0.3
}
registerS3method("sanity_check", "LearnerClassif", sanity_check.PredictionClassif)


sanity_check.PredictionRegr = function(prediction) {
  prediction$score(mlr3::msr("regr.mse")) <= 2
}
registerS3method("sanity_check", "LearnerRegr", sanity_check.PredictionRegr)

run_experiment = function(task, learner) {
  err = function(info, ...) {
    info = sprintf(info, ...)
    list(
      ok = FALSE,
      task = task, learner = learner, prediction = prediction, score = score,
      error = sprintf("[%s] learner '%s' on task '%s' failed: %s",
        stage, learner$id, task$id, info)
    )
  }

  task = mlr3::assert_task(mlr3::as_task(task))
  learner = mlr3::assert_learner(mlr3::as_learner(learner, clone = TRUE))
  mlr3::assert_learnable(task, learner)
  prediction = NULL
  score = NULL
  learner$encapsulate = c(train = "evaluate", predict = "evaluate")

  stage = "train()"
  ok = try(learner$train(task), silent = TRUE)
  if (inherits(ok, "try-error"))
    return(err(as.character(ok)))
  log = learner$log[stage == "train"]
  if ("error" %in% log$class)
    return(err("train log has errors: %s", mlr3misc::str_collapse(log[class == "error", msg])))
  if (is.null(learner$model))
    return(err("model is NULL"))

  stage = "predict()"

  if (grepl("reordered", task$id)) {
    task$col_roles$feature = rev(task$col_roles$feature)
  }

  prediction = try(learner$predict(task), silent = TRUE)
  if (inherits(ok, "try-error"))
    return(err(as.character(ok)))
  log = learner$log[stage == "predict"]
  if ("error" %in% log$class)
    return(err("predict log has errors: %s", mlr3misc::str_collapse(log[class == "error", msg])))
  msg = checkmate::check_class(prediction, "Prediction")
  if (!isTRUE(msg))
    return(err(msg))
  if (prediction$task_type != learner$task_type)
    return(err("learner and prediction have different task_type"))

  expected = mlr3::mlr_reflections$learner_predict_types[[learner$task_type]][[learner$predict_type]]
  msg = checkmate::check_subset(expected, prediction$predict_types, empty.ok = FALSE)
  if (!isTRUE(msg))
    return(err(msg))

  if(learner$predict_type == "response"){
    msg = checkmate::check_set_equal(learner$predict_type, prediction$predict_types)
    if (!isTRUE(msg))
      return(err(msg))
  } else {
    msg = checkmate::check_subset(learner$predict_type, prediction$predict_types, empty.ok = FALSE)
    if (!isTRUE(msg))
      return(err(msg))
  }

  stage = "score()"
  score = try(prediction$score(mlr3::default_measures(learner$task_type)), silent = TRUE)
  if (inherits(score, "try-error"))
    return(err(as.character(score)))
  msg = checkmate::check_numeric(score, any.missing = FALSE)
  if (!isTRUE(msg))
    return(err(msg))

  # run sanity check on sanity task
  if (grepl("^sanity", task$id) && !sanity_check(prediction)) {
    return(err("sanity check failed"))
  }

  if (grepl("^feat_all", task$id) && "importance" %in% learner$properties) {
    importance = learner$importance()
    msg = checkmate::check_numeric(rev(importance), any.missing = FALSE, min.len = 1L, sorted = TRUE)
    if (!isTRUE(msg))
      return(err(msg))
    msg = checkmate::check_names(names(importance), subset.of = task$feature_names)
    if (!isTRUE(msg))
      return(err(msg))
    if ("unimportant" %in% head(names(importance), 1L))
      return(err("unimportant feature is important"))
  }

  if (grepl("^feat_all", task$id) && "selected_features" %in% learner$properties) {
    selected = learner$selected_features()
    msg = checkmate::check_subset(selected, task$feature_names)
    if (!isTRUE(msg))
      return(err(msg))
  }

  if (grepl("^feat_all", task$id) && "oob_error" %in% learner$properties) {
    err = learner$oob_error()
    msg = checkmate::check_number(err)
    if (!isTRUE(msg))
      return(err(msg))
  }

  return(list(ok = TRUE, learner = learner, prediction = prediction, error = character()))
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

#' @title Generate Tasks for a Learner
#'
#' @description
#' Generates multiple tasks for a given [Learner], based on its properties.
#' This function is primarily used for unit tests, but can also assist while
#' writing custom learners.
#'
#' @param learner ([Learner]).
#' @param N (`integer(1)`). Number of rows of generated tasks.
#'
#' @return (`list()`) of [Task].
#' @keywords internal
#' @export
#' @examples
#' tasks = generate_tasks(mlr_learners$get("classif.rpart"))
#' tasks$missings_binary$data()
generate_tasks = function(learner, N = 20L) {
  N = assert_int(N, lower = 10L, coerce = TRUE)
  UseMethod("generate_tasks")
}

#' @export
generate_tasks.LearnerClassif = function(learner, N = 20L) {
  tasks = list()

  # generate binary tasks
  if ("twoclass" %in% learner$properties) {
    target = factor(rep_len(head(LETTERS, 2L), N))
    data = cbind(data.table(target = target), generate_data(learner, N))
    task = TaskClassif$new("proto", as_data_backend(data), target = "target", positive = "A")
    gen_tasks = generate_generic_tasks(learner, task)
    #set names
    lapply(gen_tasks, function(x) x$id = paste0(x$id, "_binary"))
    gen_tasks = set_names(gen_tasks, paste0(names(gen_tasks), "_binary"))
    tasks = c(tasks, gen_tasks)
  }

  # generate multiclass tasks
  if ("multiclass" %in% learner$properties) {
    target = factor(rep_len(head(LETTERS, 3L), N))
    data = cbind(data.table(target = target), generate_data(learner, N))
    task = TaskClassif$new("proto", as_data_backend(data), target = "target")
    gen_tasks = generate_generic_tasks(learner, task)
    #set names
    lapply(gen_tasks, function(x) x$id = paste0(x$id, "_multiclass"))
    gen_tasks = set_names(gen_tasks, paste0(names(gen_tasks), "_multiclass"))
    tasks = c(tasks, gen_tasks)
  }

  # generate sanity task
  set.seed(100)
  data = data.table(x = c(rnorm(100, 0, 1), rnorm(100, 10, 1)), y = rep(c("A", "B"), each = 100))
  task = set_names(list(TaskClassif$new("sanity", as_data_backend(data), target = "y")), "sanity")
  tasks = c(tasks, task)

  tasks
}

#' @export
generate_tasks.LearnerRegr = function(learner, N = 20L) {
  target = rnorm(N)
  data = cbind(data.table(target = target), generate_data(learner, N))
  task = TaskRegr$new("proto", as_data_backend(data), target = "target")

  tasks = generate_generic_tasks(learner, task)

  # generate sanity task
  set.seed(100)
  data = data.table(x = c(rnorm(100, 0, 1), rnorm(100, 10, 1)), y = 1)
  task = set_names(list(TaskRegr$new("sanity", as_data_backend(data), target = "y")), "sanity")
  tasks = c(tasks, task)
}

generate_generic_tasks = function(learner, task) {
  tasks = list()

  # task with all supported feature types
  sel = task$feature_types[list(learner$feature_types), "id", on = "type", with = FALSE][[1L]]
  tasks$feat_all = task$clone()$select(sel)

  if (length(task$feature_names) > 1L) {
    # individual tasks with each supported feature type
    for (type in learner$feature_types) {
      sel = task$feature_types[type, "id", on = "type", with = FALSE][[1L]]
      tasks[[sprintf("feat_%s", type)]] = task$clone()$select(sel)
    }
  }

  # task with missing values
  if ("missings" %in% learner$properties) {
    # one missing val in each feature
    features = task$feature_names
    rows = sample(task$nrow, length(features))
    data = task$data(cols = features)
    for (j in seq_along(features))
      set(data, rows[j], features[j], NA)
    tasks$missings = task$clone()$replace_features(data)

    # no row with no missing -> complete.cases() won't help
    features = sample(features, task$nrow, replace = TRUE)
    data = task$data(cols = task$feature_names)
    for (i in seq_along(features))
      set(data, i = i, j = features[i], NA)
    tasks$missings_each_row = task$clone()$replace_features(data)
  }

  # make sure that task ids match list names
  imap(tasks, function(x, n) { x$id = n; x })
}

generate_data = function(learner, N) {
  generate_feature = function(type) {
    switch(type,
      logical = rep_len(c(TRUE, FALSE), N),
      integer = rep_len(1:3, N),
      numeric = runif(N),
      character = rep_len(letters[1:2], N),
      factor = factor(rep_len(c("f1", "f2"), N), levels = c("f1", "f2")),
      ordered = ordered(rep_len(c("o1", "o2"), N), levels = c("o1", "o2"))
    )
  }
  types = unique(learner$feature_types)
  do.call(data.table, set_names(map(types, generate_feature), types))
}


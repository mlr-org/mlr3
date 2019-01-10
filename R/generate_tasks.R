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
#' tasks$missings$data()
generate_tasks = function(learner, N = 20L) {
  N = assert_int(N, lower = 10L, coerce = TRUE)
  UseMethod("generate_tasks")
}

#' @export
generate_tasks.LearnerClassif = function(learner, N = 20L) {
  binary = ("twoclass" %in% learner$properties)
  target = factor(rep_len(head(LETTERS, 2L + !binary), N))
  data = cbind(data.table(target = target), generate_data(learner, N))
  task = TaskClassif$new("proto", as_data_backend(data), target = "target", positive = if (binary) "A" else NULL)

  generate_generic_tasks(learner, task)
}

#' @export
generate_tasks.LearnerRegr = function(learner, N = 20L) {
  target = rnorm(N)
  data = cbind(data.table(target = target), generate_data(learner, N))
  task = TaskRegr$new("proto", as_data_backend(data), target = "target")

  generate_generic_tasks(learner, task)
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

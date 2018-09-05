#' @title Experiment
#'
#' @description
#' Container object for machine learning experiments.
#'
#' @export
#' @examples
#' e = Experiment$new(
#'   task = mlr_tasks$get("iris"),
#'   learner = mlr_learners$get("classif.rpart")
#' )
Experiment = R6Class("Experiment",
  public = list(
    data = NULL,

    initialize = function(task, learner, ...) {
      self$data = vector("list", nrow(reflections$experiment_slots))
      names(self$data) = reflections$experiment_slots$name

      self$data$task = assert_task(task)
      self$data$learner = assert_learner(learner)
      if (...length()) {
        dots = list(...)
        assert_names(names(dots), subset.of = names(self$data))
        self$data = insert(self$data, dots)
      }
    },

    print = function(...) {
      experiment_print(self)
    },

    train = function(subset = NULL) {
      experiment_train(self, self$data$task$row_ids(subset))
    },

    predict = function(subset = NULL, newdata = NULL) {
      experiment_predict(self, row_ids = self$data$task$row_ids(subset), newdata = newdata)
    },

    score = function() {
      experiment_score(self)
    }
  ),

  active = list(
    model = function() {
      model = self$data$learner$model
      if (is.null(model))
        stopf("No model available")
      model
    },

    logs = function() {
      list(train = self$data$train_log, test = self$data$test_log)
    },

    train_set = function() {
      resampling = self$data$resampling
      iteration = self$data$iteration
      if (is.null(resampling) || is.null(iteration))
        stopf("No train_set available")
      resampling$train_set(iteration)
    },

    test_set = function() {
      resampling = self$data$resampling
      iteration = self$data$iteration
      if (is.null(resampling) || is.null(iteration))
        stopf("No test_set available")
      resampling$test_set(iteration)
    },

    validation_set = function() {
      role = NULL
      task$row_info[role == "validation", "id"][[1L]]
    },

    predictions = function() {
      predicted = self$data$predicted
      if (is.null(predicted))
        stopf("No predictions available")
      row_ids = self$data$resampling$test_set(self$data$iteration)
      data.table(
        id = row_ids,
        truth = self$data$task$truth(row_ids)[[1L]], predicted = self$data$predicted,
        key = "id"
      )
    },

    performance = function() {
      self$data$performance
    },

    has_errors = function() {
      train_log = self$data$train_log
      test_log = self$data$test_log
      type = NULL

      (!is.null(train_log) && train_log[type == "error", .N] > 0L) ||
      (!is.null(test_log) && test_log[type == "error", .N] > 0L)
    },

    state = function() {
      d = self$data
      states = capabilities$experiment_states
      if (!is.null(d$performance))
        return(ordered("scored", levels = states))
      if (!is.null(d$predicted))
        return(ordered("predicted", levels = states))
      if (!is.null(d$learner$model))
        return(ordered("trained", levels = states))
      return(ordered("defined", levels = states))
    }
  )
)

experiment_print = function(e) {
  data = e$data
  tick = crayon::green(clisymbols::symbol$tick)
  cross = crayon::red(clisymbols::symbol$cross)

  fmt = function(x, obj, info) {
    if (is.null(x)) {
      sprintf(" %s %s", cross, obj)
    } else {
      sprintf(" %s %s: %s", tick, obj, info)
    }
  }

  catf("Experiment [%s]:", if (e$state == "scored") "complete" else "incomplete")
  catf(fmt(data$task, "Task", data$task$id))
  catf(fmt(data$learner, "Learner", data$learner$id))
  catf(fmt(data$learner$model, "Model", sprintf("[%s]", class(data$learner$model)[[1L]])))
  catf(fmt(data$predicted, "Predictions", sprintf("[%s]", class(data$predicted)[[1L]])))
  catf(fmt(data$performance, "Performance", paste(names(data$performance), signif(as.numeric(data$performance)), sep = "=", collapse = ", ")))
  catf(stri_list("\nPublic: ", setdiff(ls(e), c("initialize", "print"))))
}


experiment_train = function(e, row_ids) {
  e$data$resampling = ResamplingCustom$new()$instantiate(e$data$task, train_sets = list(row_ids))
  e$data$iteration = 1L

  value = futureCall(train_worker, list(e = e), globals = FALSE)
  e$data = insert(e$data, value(value))
  e$data = insert(e$data, list(test_time = NULL, test_log = NULL, predicted = NULL, performance = NULL))
  return(e)
}

experiment_predict = function(e, row_ids = NULL, newdata = NULL) {
  if (!is.null(row_ids) && !is.null(newdata))
    stopf("Arguments 'row_ids' and 'newdata' are mutually exclusive")

  if (is.null(newdata)) {
    e$data$resampling$instantiate(e$data$task, test_sets = list(row_ids))
  } else {
    backend = BackendDataTable$new(data = newdata, primary_key = e$data$task$backend[[1L]]$primary_key)
    e$data$task = e$data$task$clone()$add_backend(backend)
    row_ids = e$data$task$row_info[role == "validation", "id"][[1L]]
  }

  value = futureCall(predict_worker, list(e = e), globals = FALSE)
  e$data = insert(e$data, value(value))
  e$data = insert(e$data, list(performance = NULL))
  return(e)
}

experiment_score = function(e) {
  value = futureCall(score_worker, list(e = e))
  e$data = insert(e$data, value(value))
  return(e)
}


combine_experiments = function(x) {
  name = atomic = NULL
  nn = names(x[[1L]])
  encapsulate = reflections$experiment_slots[name %in% nn & atomic == FALSE, "name"][[1L]]
  rbindlist(lapply(x, function(exp) {
    exp[encapsulate] = lapply(exp[encapsulate], list)
    exp
  }))
}

assert_experiment = function(experiment) {
  assert_r6(experiment, "Experiment")
}

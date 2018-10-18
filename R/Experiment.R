#' @title Experiment
#'
#' @description
#' Container object for machine learning experiments.
#'
#' @section Usage:
#'
#' ```
#' e = Experiment$new(task, learner, ...)
#' e$train(subset)
#' e$predict(subset, newdata)
#' e$score(measures = NULL)
#'
#' e$model
#' e$prediction
#' e$performance
#'
#' e$train_set
#' e$test_set
#' e$validation_set
#' e$timings
#' e$state
#'
#' e$data
#' ```
#'
#' @section Arguments:
#' * `task` ([Task]): Task to conduct experiment on
#' * `learner` ([Learner]): Learner to conduct experiment with.
#' * `subset` (`integer` | `character`): Subset of the task's row ids to work on.
#' * `newdata` (`data.frame`): New data to predict on. Will be added to the task.
#' * `measures` (list of [Measure]): Performance measure to use. Defaults to the measures
#'    set in the [Task].
#'
#' @section Details:
#' `$new()` initializes a new machine learning experiment which can grow in a stepwise fashion.
#'
#' `$train()` fits the induces `learner` on the (subset of the) `task` and internally stores the model.
#'  The model can be accessed via `e$model`.
#'
#' `$predict()` uses the previously fitted model to predict new observations.
#'  The predictions are stored internally as an [Prediction] object and can be accessed via `e$prediction`.
#'
#' `$score()` quantifies stored predictions using the provided list of [Measure] (or the task's [Measure] if not provided)
#'  and stores the resulting performance values. The performance can be accessed via `e$performance`.
#'
#' `$train_set` and `test_set` return the row ids of the training set or test set, respectively.
#' If there is a validation set (see [Task]), `validation_set` returns the corresponding row ids.
#'
#' `$timings` holds the elapsed time for the steps `train`, `predict` and `score` in seconds with up to millisecond accuracy (see [base::proc.time()]).
#'  Timings are `NA` if the step has not been performed yet.
#'
#' `$state` returns an factor of length 1 with ordered levels `"defined"`, `"trained"`, `"predicted"` and `"scored"`.
#'
#' `$data` stores the internal representation of an Experiment as a `named list` with the following slots:
#'   * task: [Task].
#'   * learner: [Learner].
#'   * resampling: [Resampling]. Is `NULL` prior to calling `$train()`.
#'      If the experiment is constructed manually (i.e., not via [resample()] or [benchmark()],
#'      a `ResamplingCustom` object is stored here.
#'   * iteration: `integer(1)`. If the experiment is constructed manually, this is always 1.
#'   * model: Trained model as returned by the [Learner].
#'   * train_log: Log for the training step.
#'   * train_time: `numeric(1)`. Elapsed time in microseconds.
#'   * predict_log: Log for the predict step.
#'   * predict_time: `numeric(1)`. Elapsed time in microseconds.
#'   * prediction: [Prediction].
#'   * measures: list of [Measure]. Used performance measures.
#'   * performance: `named numeric`. Depending on the column `measures`.
#'   * score_time: `numeric(1)`. Elapsed time in microseconds.
#'
#' @name Experiment
#' @export
#' @examples
#' set.seed(123)
#' e = Experiment$new(
#'   task = mlr_tasks$get("iris"),
#'   learner = mlr_learners$get("classif.rpart")
#' )
#' print(e)
#' e$state
#'
#' e$train(subset = 1:120)
#' print(e)
#' e$state
#' e$model
#'
#' e$predict(subset = 121:150)
#' print(e)
#' e$state
#' e$prediction
#'
#' e$score()
#' print(e)
#' e$state
#' e$performance
#'
#' e$train_set
#' e$test_set
#' e$timings
NULL

Experiment = R6Class("Experiment",
  public = list(
    data = NULL,

    initialize = function(task, learner, ...) {
      self$data = named_list(reflections$experiment_slots$name)
      self$data$task = assert_task(task)
      self$data$learner = assert_learner(learner, task = task)
      if (...length()) {
        dots = list(...)
        assert_names(names(dots), subset.of = names(self$data))
        self$data = insert(self$data, dots)
      }
    },

    print = function(...) {
      experiment_print(self)
    },

    train = function(subset = NULL, ctrl = exec_control()) {
      experiment_train(self, self$data$task$row_ids(subset), ctrl = ctrl)
      invisible(self)
    },

    predict = function(subset = NULL, newdata = NULL, ctrl = exec_control()) {
      experiment_predict(self, row_ids = self$data$task$row_ids(subset), newdata = newdata, ctrl = ctrl)
      invisible(self)
    },

    score = function(measures = NULL, ctrl = exec_control()) {
      experiment_score(self, measures, ctrl = ctrl)
      invisible(self)
    }
  ),

  active = list(
    model = function() {
      model = self$data$model
      if (is.null(model))
        stopf("No model available")
      model
    },

    timings = function() {
      t = vnapply(self$data[c("train_time", "predict_time", "score_time")], function(x) x %??% NA_real_, use.names = FALSE)
      setNames(t, c("train", "predict", "score"))
    },

    logs = function() {
      list(train = self$data$train_log, test = self$data$predict_log)
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
      self$data$task$row_roles$validation
    },

    prediction = function() {
      prediction = self$data$prediction
      if (is.null(prediction))
        stopf("No predictions available")
      row_ids = self$test_set
      rcbind(x = data.table(id = row_ids, truth = self$data$task$truth(row_ids)[[1L]], key = "id"), y = as.data.table(prediction))[]
    },

    performance = function() {
      unlist(self$data$performance, use.names = TRUE)
    },

    has_errors = function() {
      train_log = self$data$train_log
      predict_log = self$data$predict_log

      (!is.null(train_log) && train_log$has_condition("error")) ||
      (!is.null(predict_log) && predict_log$has_condition("error"))
    },

    state = function() {
      experiment_state(self)
    }
  )
)

experiment_print = function(e) {
  data = e$data

  fmt = function(x, obj, info) {
    if (is.null(x)) {
      sprintf(" - %s: [missing]", obj)
    } else {
      sprintf(" + %s: %s", obj, info)
    }
  }

  catf("Experiment [%s]:", if (e$state == "scored") "complete" else "incomplete")
  catf(fmt(data$task, "Task", data$task$id))
  catf(fmt(data$learner, "Learner", data$learner$id))
  catf(fmt(data$model, "Model", sprintf("[%s]", class(data$model)[[1L]])))
  catf(fmt(data$prediction, "Predictions", sprintf("[%s]", class(data$prediction)[[1L]])))
  catf(fmt(data$performance, "Performance", paste(names(data$performance), signif(as.numeric(data$performance)), sep = "=", collapse = ", ")))
  catf(stri_wrap(initial = "\nPublic: ", setdiff(ls(e), c("initialize", "print"))))
}


experiment_train = function(e, row_ids, ctrl = exec_control()) {
  e$data$resampling = ResamplingCustom$new()$instantiate(e$data$task, train_sets = list(row_ids))
  e$data$iteration = 1L

  if (use_future(ctrl)) {
    debug("Running train_worker() via futureCall()")
    value = future::futureCall(train_worker, list(e = e, ctrl = ctrl), globals = FALSE, packages = "mlr3")
    value = future::value(value)
  } else {
    debug("Running train_worker()")
    value = train_worker(e, ctrl = ctrl)
  }
  e$data = insert(e$data, value)
  e$data = insert(e$data, named_list(reflections$experiment_slots[get("state") > "trained", "name"][[1L]]))
  return(e)
}

experiment_predict = function(e, row_ids = NULL, newdata = NULL, ctrl = exec_control()) {
  if (!is.null(row_ids) && !is.null(newdata))
    stopf("Arguments 'row_ids' and 'newdata' are mutually exclusive")

  if (is.null(newdata)) {
    e$data$resampling$instantiate(e$data$task, test_sets = list(row_ids))
  } else {
    e$data$task = e$data$task$clone()$rbind(newdata)
    row_ids = e$validation_set
  }

  if (use_future(ctrl)) {
    debug("Running predict_worker() via futureCall()")
    value = future::futureCall(predict_worker, list(e = e, ctrl = ctrl), globals = FALSE, packages = "mlr3")
    value = future::value(value)
  } else {
    debug("Running predict_worker()")
    value = predict_worker(e, ctrl = ctrl)
  }
  e$data = insert(e$data, value)
  e$data = insert(e$data, named_list(reflections$experiment_slots[get("state") > "predicted", "name"][[1L]]))
  return(e)
}

experiment_score = function(e, measures = NULL, ctrl = exec_control()) {
  e$data$measures = assert_measures(measures %??% e$data$task$measures, task = e$data$task, learner = e$data$learner)

  if (use_future(ctrl)) {
    debug("Running score_worker() via futureCall()")
    value = future::futureCall(score_worker, list(e = e, ctrl = ctrl), globals = FALSE, packages = "mlr3")
    value = future::value(value)
  } else {
    debug("Running score_worker()")
    value = score_worker(e, ctrl = ctrl)
  }

  e$data = insert(e$data, value)
  return(e)
}

experiment_state = function(self) {
  d = self$data
  states = levels(reflections$experiment_slots$state)
  if (!is.null(d$performance))
    return(ordered("scored", levels = states))
  if (!is.null(d$prediction))
    return(ordered("predicted", levels = states))
  if (!is.null(d$model))
    return(ordered("trained", levels = states))
  return(ordered("defined", levels = states))
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

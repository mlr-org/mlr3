#' @title Experiment
#'
#' @name Experiment
#' @format [R6Class] object.
#' @description
#' Container object for machine learning experiments.
#'
#' @section Usage:
#' ```
#' # Construction
#' e = Experiment$new(task, learner, ...)
#'
#' # Members
#' e$ctrl
#' e$data
#' e$has_errors
#' e$hash
#' e$learner
#' e$logs
#' e$model
#' e$performance
#' e$prediction
#' e$state
#' e$task
#' e$test_set
#' e$timings
#' e$train_set
#' e$validation_set
#'
#' # Methods
#' e$predict(row_ids, newdata, ctrl = list())
#' e$score(ctrl = list())
#' e$train(row_ids, ctrl = list())
#' ```
#'
#' @section Arguments:
#' * `task` ([Task]): Task to conduct experiment on
#' * `learner` ([Learner]): Learner to conduct experiment with.
#' * `row_ids` (`integer()` | `character()`): Subset of the task's row ids to work on. Invalid row ids are silently ignored.
#' * `newdata` ([data.frame]): New data to predict on. Will be appended to the task.
#'
#' @section Details:
#' * `$new()` initializes a new machine learning experiment which can grow in a stepwise fashion.
#' * `$predict()` uses the previously fitted model to predict new observations.
#'   The predictions are stored internally as an [Prediction] object and can be
#'   accessed via `e$prediction` as [data.table()].
#' * `$score()` quantifies stored predictions using the task's [Measure] and stores the resulting
#'   performance values. The performance can be accessed via `e$performance`.
#' * `$train()` fits the induced [Learner] on the (subset of the) `task` and'stores the model in the [Learner]. The model can be accessed via `e$model`.
#' * `$ctrl` ([list]). List of control settings passed to `$train()`, `$predict()` and `$score()`.
#' * `$data` stores the internal representation of an Experiment as a `named list` with the following slots:
#'   * iteration (`integer(1)`). If the experiment is constructed manually, this is always 1.
#'   * learner ([Learner]).
#'   * measures (`list` of [Measure]). Measures used for performance assessment.
#'   * performance (`named numeric`). Performance values returned by the measures.
#'   * predict_log. [Log] for the predict step.
#'   * predict_time (`numeric(1)`). Elapsed time in microseconds.
#'   * prediction ([Prediction]).
#'   * resampling ([Resampling]). Is `NULL` prior to calling `$train()`. If the experiment is constructed manually (i.e., not via [resample()] or [benchmark()], a `ResamplingCustom` object is stored here.
#'   * score_time (`numeric(1)`). Elapsed time in microseconds.
#'   * task ([Task]).
#'   * train_log: [Log] for the training step.
#'   * train_time (`numeric(1)`). Elapsed time in microseconds.
#' * `$has_errors` (`logical(1)`). Whether the Experiment showed errors either during training or prediction.
#' * `$hash` (`character(1)`). The hash of the experiment.
#' * `$logs` (named `list(2)`) returns a list with names `train` and `predict`.
#'   Both store an object of class [Log] if logging of the learner has been
#'   enabled via [mlr_control()], and are `NULL` if logging was disabled or the
#'   respective step has not been performed yet.
#' * `$state` (`ordered(1)`) returns the state of the experiment: `"defined"`,
#'   `"trained"`, `"predicted"`, or `"scored"`.
#' * `$task` and `$learner` can be used to access the [Task] and [Learner].
#' * `$timings` (named `numeric(3)`) holds the elapsed time for the steps
#'   `train`, `predict` and `score` in seconds with up to millisecond accuracy
#'   (c.f. [proc.time()]). Timings are `NA` if the respective step has not been
#'   performed yet.
#' * `$train_set` and `$test_set` (`integer()` | `character()`) return the row
#'   ids of the training set or test set, respectively.
#' * `$validation_set` (`integer()` | `character()`) returns the row ids of the
#'   validation set (see [Task]).
#'
#' @examples
#' e = Experiment$new(
#'   task = mlr_tasks$get("iris"),
#'   learner = mlr_learners$get("classif.rpart")
#' )
#' print(e)
#' e$state
#'
#' e$train(row_ids = 1:120)
#' print(e)
#' e$state
#' e$model
#'
#' e$predict(row_ids = 121:150)
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
NULL

#' @export
Experiment = R6Class("Experiment",
  public = list(
    data = NULL,
    ctrl = NULL,

    initialize = function(task = NULL, learner = NULL, ctrl = list()) {
      self$data = named_list(mlr_reflections$experiment_slots$name)
      if (!is.null(task))
        self$data$task = assert_task(task)$clone(deep = TRUE)
      if (!is.null(learner))
        self$data$learner = assert_learner(learner, task = task)$clone(deep = TRUE)
      self$ctrl = assert_list(ctrl)
    },

    format = function() {
      "<Experiment>"
    },

    print = function(...) {
      experiment_print(self)
    },

    train = function(row_ids = NULL, ctrl = list()) {
      if (! self$state >= "defined")
        stopf("Experiment needs a task and a learner")
      experiment_train(self, row_ids = row_ids %??% self$data$task$row_ids, ctrl = ctrl)
      invisible(self)
    },

    predict = function(row_ids = NULL, newdata = NULL, ctrl = list()) {
      if (! self$state >= "trained")
        stopf("Experiment needs to be trained before predict()")
      if (!is.null(row_ids) && !is.null(newdata))
        stopf("Arguments 'row_ids' and 'newdata' are mutually exclusive")
      experiment_predict(self, row_ids = row_ids %??% self$data$task$row_ids, newdata = newdata, ctrl = ctrl)
      invisible(self)
    },

    score = function(ctrl = list()) {
      if (! self$state >= "trained")
        stopf("Experiment needs predictions before score()")
      experiment_score(self, ctrl = ctrl)
      invisible(self)
    }
  ),

  active = list(
    task = function(rhs) {
      if (missing(rhs))
        return(self$data$task)
      self$data$task = assert_task(rhs)$clone(deep = TRUE)
    },

    learner = function(rhs) {
      if (missing(rhs))
        return(self$data$learner)
      self$data$learner = assert_learner(rhs)$clone(deep = TRUE)
    },

    model = function() {
      learner = self$data$learner
      learner$model %??% learner$fallback$model
    },

    timings = function() {
      t = map_dbl(self$data[c("train_time", "predict_time", "score_time")], function(x) x %??% NA_real_)
      set_names(t, c("train", "predict", "score"))
    },

    logs = function() {
      list(train = self$data$train_log, predict = self$data$predict_log)
    },

    train_set = function() {
      resampling = self$data$resampling
      iteration = self$data$iteration
      if (is.null(resampling) || is.null(iteration))
        return(NULL)
      resampling$train_set(iteration)
    },

    test_set = function() {
      resampling = self$data$resampling
      iteration = self$data$iteration
      if (is.null(resampling) || is.null(iteration))
        return(NULL)
      resampling$test_set(iteration)
    },

    validation_set = function() {
      self$data$task$row_roles$validation
    },

    prediction = function() {
      self$data$prediction
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
    },

    hash = function() {
      if (is.na(private$.hash))
        private$.hash = experiment_data_hash(self$data)
      private$.hash
    }
  ),

  private = list(
    .hash = NA_character_
  )
)

experiment_data_hash = function(data) {
  hash(c(data$task$hash, data$learner$hash, data$resampling$hash))
}

experiment_print = function(self) {
  data = self$data

  fmt = function(x, obj, info) {
    if (is.null(x)) {
      sprintf(" - %s: [missing]", obj)
    } else {
      sprintf(" + %s: %s", obj, info)
    }
  }

  catf("%s [%s]:", format(self), self$state)
  catf(fmt(data$task, "Task", data$task$id))
  catf(fmt(data$learner, "Learner", data$learner$id))
  catf(fmt(self$model, "Model", sprintf("[%s]", class(self$model)[[1L]])))
  catf(fmt(data$prediction, "Predictions", sprintf("[%s]", class(data$prediction)[[1L]])))
  catf(fmt(data$performance, "Performance", paste(names(data$performance), format(as.numeric(data$performance)), sep = "=", collapse = ", ")))
  catf(str_indent("\nPublic:", str_r6_interface(self)))
}


experiment_train = function(self, row_ids, ctrl = list()) {
  ctrl = mlr_control(insert_named(self$ctrl, ctrl))
  self$data$resampling = ResamplingCustom$new()$instantiate(self$data$task, train_sets = list(row_ids))
  self$data$iteration = 1L

  log_info("Training learner '%s' on task '%s' ...", self$learner$id, self$task$id, namespace = "mlr3")
  value = train_worker(self, ctrl = ctrl)

  self$data = insert_named(self$data, value)
  return(experiment_reset_state(self, "trained"))
}

experiment_predict = function(self, row_ids = NULL, newdata = NULL, ctrl = list()) {
  ctrl = mlr_control(insert_named(self$ctrl, ctrl))

  if (is.null(newdata)) {
    self$data$resampling$instantiate(self$data$task, test_sets = list(row_ids))
  } else {
    self$data$task = self$data$task$clone()$rbind(newdata)
    row_ids = self$validation_set
  }

  # FIXME: learner id
  log_info("Predicting with model of learner '%s' on task '%s' ...", self$learner$id, self$task$id, namespace = "mlr3")
  value = predict_worker(self, ctrl = ctrl)

  self$data = insert_named(self$data, value)
  return(experiment_reset_state(self, "predicted"))
}

experiment_score = function(self, ctrl = list()) {
  ctrl = mlr_control(insert_named(self$ctrl, ctrl))
  self$data$measures = assert_measures(self$data$task$measures, task = self$task, prediction = self$prediction)

  log_info("Scoring predictions of learner '%s' on task '%s' ...", self$learner$id, self$task$id, namespace = "mlr3")
  value = score_worker(self, ctrl = ctrl)

  self$data = insert_named(self$data, value)
  return(self)
}

combine_experiments = function(x) {
  name = atomic = NULL
  nn = names(x[[1L]])
  wrap_list = mlr_reflections$experiment_slots[name %in% nn & atomic == FALSE, "name"][[1L]]
  map_dtr(x, function(exp) {
    exp[wrap_list] = lapply(exp[wrap_list], list)
    exp
  })
}

experiment_state = function(self) {
  as_state = function(state) ordered(state, levels = mlr_reflections$experiment_states)
  d = self$data

  if (!is.null(d$score_time))
    return(as_state("scored"))
  if (!is.null(d$predict_time))
    return(as_state("predicted"))
  if (!is.null(d$train_time))
    return(as_state("trained"))
  if (!is.null(d$task) && !is.null(d$learner))
    return(as_state("defined"))
  return(as_state("undefined"))
}

experiment_reset_state = function(self, new_state) {
  slots = mlr_reflections$experiment_slots[get("state") > new_state, "name", with = FALSE][[1L]]
  self$data[slots] = list(NULL)
  self
}

# creates an experiment with the data provided via ...
# arguments are **not** cloned
# extra args which do not belong in an experiment are removed
as_experiment = function(...) {
  e = Experiment$new()
  dots = list(...)
  e$data[match(names(dots), names(e$data), nomatch = 0L)] = dots
  e
}

#' @title Experiment
#'
#' @usage NULL
#' @format [R6::R6Class] object.
#' @include mlr_reflections.R
#'
#' @description
#' Container object for a machine learning experiment.
#' After initialization with a [Task] and a [Learner], the experiment is conducted
#' by calling the methods `$train()`, `$predict()` and `$score()`.
#'
#' @section Construction:
#' ```
#' Experiment$new(task = NULL, learner = NULL, ctrl = list())
#' ```
#'
#' * `task` :: ([Task] | `character(1)`)\cr
#'   May be `NULL` during initialization, but is mandatory to train the Experiment.
#'   Instead of a [Task] object, it is also possible to provide a key to retrieve a task from the [mlr_tasks] dictionary.
#'
#' * `learner` :: [Learner] | `character(1)`)\cr
#'   May be `NULL` during initialization, but is mandatory to train the Experiment.
#'   Instead of a [Learner] object, it is also possible to provide a key to retrieve a learner from the [mlr_learners] dictionary.
#'
#' * `ctrl` :: named `list()`\cr
#'   Control object, see [mlr_control()].
#'
#' @section Fields:
#' * `ctrl` :: `list()`\cr
#'   Control settings passed during initialization.
#'
#' * `data` :: named `list()`\cr
#'   See section "Internal Data Storage".
#'
#' * `has_errors` :: `logical(1)`\cr
#'   Flag which is `TRUE` if any error has been recorded during `$train()` or `$predict()`.
#'
#' * `hash` :: `character(1)`\cr
#'   Hash (unique identifier) for this object.
#'
#' * `model` :: `any`\cr
#'   Access the trained model.
#'   Only available after the experiment has been trained.
#'
#' * `performance` :: named `numeric()`\cr
#'   Access the scored performance scores as returned by the [Measure] stored in the [Task].
#'
#' * `prediction` :: [Prediction]\cr
#'   Access the individual predictions of the model stored in the [Learner].
#'
#' * `seeds` :: `integer(3)`\cr
#'   Named integer of random number generator seeds passed to [set.seed()] prior to calling external code in `train()`, `predict()` or `score()`.
#'   Names must match "train", "predict" and "score". Set to `NA` to disable seeding (default).
#'
#' * `state` :: `ordered(1)`\cr
#'   Returns the state of the experiment as ordered factor with levels `"defined"`,
#'   `"trained"`, `"predicted"`, and `"scored"`.
#'
#' * `task` :: [Task]\cr
#'   Access to the stored [Task].
#'
#' * `learner` :: [Learner]\cr
#'   Access to the stored [Learner].
#'   If the experiment has been fitted, the model is stored in slot `$model`.
#'
#' * `test_set` :: (`integer()` | `character()`)\cr
#'   The row ids of the [Task] for the test set used in `$predict()`
#'   Timings are `NA` if the respective step has not been performed yet.
#'
#' * `timings` :: named `numeric(3)`\cr
#'   Stores the elapsed time for the steps `train()`, `predict()` and `score()` in seconds with up to millisecond accuracy (c.f. `proc.time()`).
#'
#' * `train_set` :: (`integer()` | `character()`)\cr
#'   The row ids of the [Task] for the training set used in `$train()`.
#'
#' * `validation_set` :: (`integer()` || `character()`)\cr
#'   The row ids of the validation set of the [Task].
#'   Validation sets are not yet completely integrated into the package.
#'
#' @section Methods:
#' * `train(row_ids = NULL, ctrl = list())`\cr
#'   (`integer()` | `character()`, `list()`) -> `self`\cr
#'   Fits the induced [Learner] on the `row_ids` of the [Task] and stores the model inside the [Learner] object.
#'   If no `row_ids` are provided, trains the model on all rows of the [Task] with row role `"use"`.
#'   The fitted model can be accessed via `$model`.
#'
#' * `predict(row_ids = NULL, newdata = NULL, ctrl = list())`\cr
#'   (`integer()` | `character()`, `data.frame()`, `list()`) -> `self`\cr
#'   Uses the previously fitted model to predict new observations.
#'   New observations are either addressed as `row_ids` referencing rows in the stored task, or
#'   as `data.frame()` via `newdata`.
#'   The later fuses the new observations with the stored [Task], and thereby mutates the Experiment.
#'   To avoid any side effects, it is advised to clone the experiment first.
#'   The resulting predictions are stored internally as an [Prediction] object and can be accessed via `$prediction`.
#'
#' * `score(measures = NULL, ctrl = list())`\cr
#'   (list of `[Measure]`, `list()`) -> `self`\cr
#'   Quantifies stored predictions using the list of [Measure] provided here,
#'   defaulting to the default measures that come with the [Task].
#'   The performance values are stored internally and can be accessed via `$performance`.
#'
#' * `log(steps = c("train", "predict"))`\cr
#'   `character(1)` -> [Log]\cr
#'   Returns a [Log] for specified steps.
#'
#'
#' @section Internal Data Storage:
#' All data is stored in the slot `data` as named `list()`.
#' Directly accessing the elements is not recommended, but sometimes required, especially if you aim to extend `mlr3`.
#' The data object contains the following items:
#'
#' * `task` :: [Task]\cr
#'   A clone of the [Task] which was provided during construction.
#'   Also accessible via `e$task`.
#'
#' * `learner` :: [Learner]\cr
#'   A clone of the [Learner] which was provided during construction.
#'   If retrieved via `e$learner`, the slot `$model` contains the fitted model.
#'
#' * `resampling` :: [Resampling]\cr
#'   Is `NULL` prior to calling `$train()`.
#'   If the experiment is constructed manually (i.e., not via [resample()] or [benchmark()]), a [ResamplingCustom] object is stored.
#'   The combination of `resampling` and `iteration` (next item) is used to extract the training and test set indices.
#'   These are directly accessible via `e$train_set` and `e$test_set`.
#'
#' * `iteration` :: `integer(1)`\cr
#'   Refers to the iteration number of the stored [Resampling] object.
#'   If the experiment is constructed manually, this is always `1`, as there is only one training-test split.
#'
#' * `train_log` :: [data.table::data.table()]\cr
#'   [Log] for the training step. May be `NULL` if no encapsulation has been enabled via [mlr_control()].
#'
#' * `train_time` :: `numeric(1)`\cr
#'   Elapsed time during train in seconds with up to millisecond accuracy (c.f. `proc.time()`).
#'
#' * `predict_log` :: [data.table::data.table()]\cr
#'   [Log] for the predict step. May be `NULL` if no encapsulation has been enabled via [mlr_control()].
#'
#' * `predict_time` :: `numeric(1)`\cr
#'   Elapsed time during predict in seconds with up to millisecond accuracy (c.f. `proc.time()`).
#'
#' * `predicted` :: named `list()`\cr
#'   Prediction as returned by the [Learner]'s `predict()` call, possibly converted by [convert_prediction()].
#'   List elements are named with predict types.
#'
#' * `measures` :: `list()` of [Measure]\cr
#'   Measures which where used for performance assessment.
#'
#' * `performance` :: named `numeric()`\cr
#'   Aggregated scores returned by the measures, named with measure ids.
#'
#' * `score_time` :: `numeric(1)`\cr
#'   Elapsed time during score in seconds with up to millisecond accuracy (c.f. `proc.time()`)..
#'
#' @export
#' @examples
#' e = Experiment$new(task = "iris", learner = "classif.rpart")
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
Experiment = R6Class("Experiment",
  public = list(
    data = NULL,
    ctrl = NULL,
    seeds = NULL,

    initialize = function(task = NULL, learner = NULL, ctrl = list()) {
      self$data = named_list(mlr_reflections$experiment_slots$name)
      if (!is.null(task)) {
        self$data$task = assert_task(task, clone = TRUE)
      }
      if (!is.null(learner)) {
        self$data$learner = assert_learner(learner, task = self$data$task, clone = TRUE)
      }
      self$ctrl = assert_list(ctrl)
      self$seeds = set_names(rep.int(NA_integer_, 3L), c("train", "predict", "score"))
    },

    format = function() {
      "<Experiment>"
    },

    print = function(...) {
      experiment_print(self)
    },

    train = function(row_ids = NULL, ctrl = list()) {
      experiment_train(self, private, row_ids = row_ids, ctrl = ctrl)
    },

    predict = function(row_ids = NULL, newdata = NULL, ctrl = list()) {
      experiment_predict(self, private, row_ids = row_ids, newdata = newdata, ctrl = ctrl)
    },

    score = function(measures = NULL, ctrl = list()) {
      experiment_score(self, private, measures, ctrl = ctrl)
    },

    log = function(steps = c("train", "predict")) {
      steps = assert_sorted_subset(steps, c("train", "predict"), empty.ok = FALSE)
      parts = set_names(self$data[sprintf("%s_log", steps)], steps)
      data = rbindlist(parts, idcol = "context", use.names = TRUE)
      if (nrow(data) == 0L) {
        return(Log$new())
      }
      Log$new(data)
    }),

  active = list(
    task = function(rhs) {
      if (missing(rhs)) {
        return(self$data$task)
      }
      self$data$task = assert_task(rhs)$clone(deep = TRUE)
    },

    learner = function(rhs) {
      if (missing(rhs)) {
        learner = self$data$learner
        model = self$model
        if (!is.null(model)) {
          learner = learner$clone()
          learner$model = model
        }
        return(learner)
      }
      self$data$learner = assert_learner(rhs)$clone(deep = TRUE)
    },

    model = function() {
      self$data$model
    },

    timings = function() {
      t = map_dbl(self$data[c("train_time", "predict_time", "score_time")], function(x) x %??% NA_real_)
      set_names(t, c("train", "predict", "score"))
    },

    train_set = function() {
      resampling = self$data$resampling
      iteration = self$data$iteration
      if (is.null(resampling) || is.null(iteration)) {
        return(NULL)
      }
      resampling$train_set(iteration)
    },

    test_set = function() {
      resampling = self$data$resampling
      iteration = self$data$iteration
      if (is.null(resampling) || is.null(iteration)) {
        return(NULL)
      }
      resampling$test_set(iteration)
    },

    validation_set = function() {
      self$data$task$row_roles$validation
    },

    prediction = function(rhs) {
      if (missing(rhs)) {
        if (is.null(self$data$predicted))
          return(NULL)
        return(invoke(as_prediction, task = self$task, row_ids = self$test_set, predicted = self$data$predicted))
      }

      assert_list(rhs, names = "unique")
      assert_names(names(rhs), subset.of = self$learner$predict_types)
      experiment_reset_state(self, "predicted")
      self$data$predicted = rhs
    },

    performance = function() {
      unlist(self$data$performance, use.names = TRUE)
    },

    has_errors = function() {
      self$log()$has_condition("error")
    },

    state = function() {
      experiment_state(self)
    },

    hash = function() {
      if (is.na(private$.hash)) {
        private$.hash = experiment_data_hash(self$data)
      }
      private$.hash
    }),

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
  catf(fmt(self$prediction, "Predictions", sprintf("[%s]", class(self$prediction)[[1L]])))
  catf(fmt(data$performance, "Performance", paste(names(data$performance), format(as.numeric(data$performance)), sep = "=", collapse = ", ")))
  catf(str_indent("\nPublic:", str_r6_interface(self)))
}


experiment_train = function(self, private, row_ids, ctrl = list()) {

  if (!self$state >= "defined") {
    stopf("Experiment needs a task and a learner")
  }

  ctrl = mlr_control(insert_named(self$ctrl, ctrl))
  row_ids = if (is.null(row_ids)) self$data$task$row_ids else assert_row_ids(row_ids)
  self$data$resampling = ResamplingCustom$new()$instantiate(self$data$task, train_sets = list(row_ids))
  self$data$iteration = 1L

  log_info("Training learner '%s' on task '%s' ...", self$learner$id, self$task$id, namespace = "mlr3")
  value = train_worker(self$task, self$learner, self$train_set, ctrl, seed = self$seeds[["train"]])

  self$data = insert_named(self$data, value)
  private$.hash = NA_character_
  experiment_reset_state(self, "trained")
}

experiment_predict = function(self, private, row_ids = NULL, newdata = NULL, ctrl = list()) {

  if (!self$state >= "trained") {
    stopf("Experiment needs to be trained before predict()")
  }
  if (!is.null(row_ids) && !is.null(newdata)) {
    stopf("Arguments 'row_ids' and 'newdata' are mutually exclusive")
  }
  ctrl = mlr_control(insert_named(self$ctrl, ctrl))

  # TODO: we could allow new_data to be a backend / task to avoid duplication
  if (!is.null(newdata)) {
    assert_data_frame(newdata)
    tn = self$task$target_names
    if (any(tn %nin% colnames(newdata))) {
      newdata[, tn] = NA
    }
    old_row_ids = self$data$task$row_ids
    self$data$task = self$data$task$clone(deep = TRUE)$rbind(newdata)
    row_ids = setdiff(self$data$task$row_ids, old_row_ids)
  } else {
    row_ids = if (is.null(row_ids)) self$task$row_ids else assert_row_ids(row_ids)
  }
  self$data$resampling$instantiate(self$data$task, test_sets = list(row_ids))

  log_info("Predicting with model of learner '%s' on task '%s' ...", self$learner$id, self$task$id, namespace = "mlr3")
  value = predict_worker(self$task, self$learner, self$data$model, self$test_set, ctrl, self$seeds[["predict"]])

  self$data = insert_named(self$data, value)
  private$.hash = NA_character_
  return(experiment_reset_state(self, "predicted"))
}

experiment_score = function(self, private, measures = NULL, ctrl = list()) {

  if (!self$state >= "trained") {
    stopf("Experiment needs predictions before score()")
  }
  ctrl = mlr_control(insert_named(self$ctrl, ctrl))
  self$data$measures = assert_measures(measures %??% self$data$task$measures, task = self$task, predict_types = names(self$data$predicted))

  log_info("Scoring predictions of learner '%s' on task '%s' ...", self$learner$id, self$task$id, namespace = "mlr3")
  value = score_worker(self, ctrl = ctrl)

  self$data = insert_named(self$data, value)
  private$.hash = NA_character_
  return(self)
}

combine_experiments = function(x) {
  name = atomic = NULL
  nn = names(x[[1L]])
  wrap_list = mlr_reflections$experiment_slots[name %in% nn & atomic == FALSE, "name"][[1L]]
  map_dtr(x, function(e) {
    e[wrap_list] = lapply(e[wrap_list], list)
    e
  })
}

experiment_state = function(self) {
  as_state = function(state) ordered(state, levels = mlr_reflections$experiment_states)
  d = self$data

  if (!is.null(d$score_time)) {
    return(as_state("scored"))
  }
  if (!is.null(d$predict_time)) {
    return(as_state("predicted"))
  }
  if (!is.null(d$train_time)) {
    return(as_state("trained"))
  }
  if (!is.null(d$task) && !is.null(d$learner)) {
    return(as_state("defined"))
  }
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
  ii = intersect(names(dots), names(e$data))
  e$data[ii] = dots[ii]
  e
}

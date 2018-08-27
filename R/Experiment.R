#' @export
#' @keywords internal
Experiment = R6Class("Experiment",
  public = list(
    data = NULL,

    initialize = function(task, learner, ...) {
      self$data = vector("list", nrow(capabilities$experiment_slots))
      names(self$data) = capabilities$experiment_slots$name

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
      train_experiment(self, subset)
    },

    predict = function(subset = NULL, newdata = NULL) {
      predict_experiment(self, subset = subset, newdata = newdata)
    },

    score = function(measures = NULL) {
      score_experiment(self, measures)
    }
  ),

  active = list(
    model = function() {
      model = self$data$model
      if (is.null(model))
        stop("No model available")
      model
    },

    logs = function() {
      list(train = self$data$train_log, test = self$data$test_log)
    },

    train_set = function() {
      resampling = self$data$resampling
      iteration = self$data$iteration
      if (is.null(resampling) || is.null(iteration))
        stop("No train_set available")
      resampling$train_set(iteration)
    },

    test_set = function() {
      resampling = self$data$resampling
      iteration = self$data$iteration
      if (is.null(resampling) || is.null(iteration))
        stop("No test_set available")
      resampling$test_set(iteration)
    },

    validation_set = function() {
      role = NULL
      row_ids = task$rows[role == "validation", "id"][[1L]]
    },

    predictions = function() {
      predicted = self$data$predicted
      if (is.null(predicted))
        stop("No predictions available")
      row_ids = self$data$resampling$test_set(1L)
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
      if (!is.null(d$model))
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
  catf(fmt(data$model, "Model", sprintf("[%s]", class(data$model)[[1L]])))
  catf(fmt(data$predicted, "Predictions", sprintf("[%s]", class(data$predicted)[[1L]])))
  catf(fmt(data$performance, "Performance", stri_paste(names(data$performance), signif(as.numeric(data$performance)), sep = "=", collapse = ", ")))
  catf(stri_list("\nPublic: ", setdiff(ls(e), c("initialize", "print"))))
}

combine_experiments = function(x) {
  nn = names(x[[1L]])
  # FIXME: NSE
  encapsulate = capabilities$experiment_slots[name %in% nn & atomic == FALSE, "name"][[1L]]
  rbindlist(lapply(x, function(exp) {
    exp[encapsulate] = lapply(exp[encapsulate], list)
    exp
  }))
}

assert_experiment = function(experiment) {
  assert_r6(experiment, "Experiment")
}

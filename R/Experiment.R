#' @export
#' @keywords internal
Experiment = R6Class("Experiment",
  public = list(
    data = NULL,

    initialize = function(task, learner, ...) {
      self$data = vector("list", nrow(capabilities$experiment.slots))
      names(self$data) = capabilities$experiment.slots$name

      self$data$task = assertTask(task)
      self$data$learner = assertLearner(learner)
      if (...length()) {
        dots = list(...)
        assertNames(names(dots), subset.of = names(self$data))
        self$data = insert(self$data, dots)
      }
    },

    print = function(...) {
      printExperiment(self)
    },

    train = function(subset = NULL) {
      trainExperiment(self, subset)
    },

    predict = function(subset = NULL, newdata = NULL) {
      predictExperiment(self, subset = subset, newdata = newdata)
    },

    score = function(measures = NULL) {
      scoreExperiment(self, measures)
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
      list(train = self$data$train.log, test = self$data$test.log)
    },

    train.set = function() {
      resampling = self$data$resampling
      iteration = self$data$iteration
      if (is.null(resampling) || is.null(iteration))
        stop("No train.set available")
      resampling$train.set(iteration)
    },

    test.set = function() {
      resampling = self$data$resampling
      iteration = self$data$iteration
      if (is.null(resampling) || is.null(iteration))
        stop("No test.set available")
      resampling$test.set(iteration)
    },

    validation.set = function() {
      role = NULL
      row.ids = task$rows[role == "validation", "id"][[1L]]
    },

    predictions = function() {
      predicted = self$data$predicted
      if (is.null(predicted))
        stop("No predictions available")
      row.ids = self$data$resampling$test.set(1L)
      data.table(
        id = row.ids,
        truth = self$data$task$truth(row.ids)[[1L]], predicted = self$data$predicted,
        key = "id"
      )
    },

    performance = function() {
      self$data$performance
    },

    has.errors = function() {
      train.log = self$data$train.log
      test.log = self$data$test.log
      type = NULL

      (!is.null(train.log) && train.log[type == "error", .N] > 0L) ||
      (!is.null(test.log) && test.log[type == "error", .N] > 0L)
    },

    state = function() {
      d = self$data
      states = capabilities$experiment.states
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

printExperiment = function(e) {
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

combineExperiments = function(x) {
  nn = names(x[[1L]])
  # FIXME: NSE
  encapsulate = capabilities$experiment.slots[name %in% nn & atomic == FALSE, "name"][[1L]]
  rbindlist(lapply(x, function(exp) {
    exp[encapsulate] = lapply(exp[encapsulate], list)
    exp
  }))
}

assertExperiment = function(experiment) {
  assertR6(experiment, "Experiment")
}

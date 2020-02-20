#' @title Measure Class
#'
#' @include mlr_reflections.R
#'
#' @description
#' This is the abstract base class for measures like [MeasureClassif] and [MeasureRegr].
#'
#' Measures are classes around tailored around two functions:
#'
#' 1. A function `score` which quantifies the performance by comparing true and predicted response.
#' 2. A function `aggregator` which combines multiple performance scores returned by
#'    `calculate` to a single numeric value.
#'
#' In addition to these two functions, meta-information about the performance measure is stored.
#'
#' Predefined measures are stored in the [mlr3misc::Dictionary] [mlr_measures],
#' e.g. [`classif.auc`][mlr_measures_classif.auc] or [`time_train`][mlr_measures_time_train].
#' A guide on how to extend \CRANpkg{mlr3} with custom measures can be found in the [mlr3book](https://mlr3book.mlr-org.com).
#'
#' Note that this object is typically constructed via a derived classes, e.g. [MeasureClassif] or [MeasureRegr].
#'
#' @template param_id
#' @template param_task_type
#' @template param_predict_type
#' @template param_measure_properties
#' @template param_packages
#' @template param_man
#'
#' @family Measure
#' @export
Measure = R6Class("Measure",
  cloneable = FALSE,
  public = list(
    #' @template field_id
    id = NULL,

    #' @template field_task_type
    task_type = NULL,

    #' @field predict_type (`character(1)`)\cr
    #' Required predict type of the [Learner].
    predict_type = NULL,

    #' @template field_predict_sets
    predict_sets = NULL,

    #' @field average (`character(1)`)\cr
    #' Method for aggregation.
    #' Either `"micro"` or `"macro"`.
    average = NULL,

    #' @field aggregator (`function()`)\cr
    #' Function to aggregate individual scores.
    aggregator = NULL,

    #' @field task_properties (`character()`)\cr
    #' Required properties of the [Task].
    task_properties = NULL,

    #' @field range (`numeric(2)`)\cr
    #' Lower and upper bound of possible performance scores.
    range = NULL,

    #' @field properties (`character()`)\cr
    #' Properties of this measure.
    properties = NULL,

    #' @field minimize (`logical(1)`)\cr
    #' If `TRUE`, good predictions correspond to small values of performance scores.
    minimize = NULL,

    #' @template field_packages
    packages = NULL,

    #' @template field_man
    man = NULL,

    #' @description
    #' Creates a new instance of the [R6][R6::R6Class] object.
    #'
    #' @param range (`numeric(2)`)\cr
    #'   Feasible range for this measure as `c(lower_bound, upper_bound)`.
    #'   Both bounds may be infinite.
    #'
    #' @param minimize (`logical(1)`)\cr
    #'   Set to `TRUE` if good predictions correspond to small values,
    #'   and to `FALSE` if good predictions correspond to large values.
    #'   If set to `NA` (default), tuning this measure is not possible.
    #'
    #' @param average (`character(1)`)\cr
    #'   How to average multiple [Prediction]s from a [ResampleResult].
    #'
    #'   The default, `"macro"`, calculates the individual performances scores for each [Prediction] and then uses the
    #'   function defined in `aggregator` to average them to a single number.
    #'
    #'   If set to `"micro"`, the individual [Prediction] objects are first combined into a single new [Prediction] object which is then used to assess the performance.
    #'   The function `aggregator` is not used in this case.
    #'
    #' @param aggregator (`function(x)`)\cr
    #'   Function to aggregate individual performance scores `x` where `x` is a numeric vector.
    #'   If `NULL`, defaults to [mean()].
    #'
    #' @param predict_sets (`character()`)\cr
    #'   Prediction sets to operate on, used in `aggregate()` to extract the matching `predict_sets` from the [ResampleResult].
    #'   Multiple predict sets are calculated by the respective [Learner] during [resample()]/[benchmark()].
    #'   Must be a non-empty subset of `c("train", "test")`.
    #'   If multiple sets are provided, these are first combined to a single prediction object.
    #'   Default is `"test"`.
    #'
    #' @param task_properties (`character()`)\cr
    #'   Required task properties, see [Task].
    initialize = function(id, task_type = NA, range = c(-Inf, Inf), minimize = NA, average = "macro", aggregator = NULL, properties = character(), predict_type = "response",
      predict_sets = "test", task_properties = character(), packages = character(), man = NA_character_) {

      self$id = assert_string(id, min.chars = 1L)
      self$task_type = task_type
      self$range = assert_range(range)
      self$minimize = assert_flag(minimize, na.ok = TRUE)
      self$average = assert_choice(average, c("macro", "micro"))
      self$aggregator = assert_function(aggregator, null.ok = TRUE)

      if (!is_scalar_na(task_type)) {
        assert_choice(task_type, mlr_reflections$task_types$type)
        assert_choice(predict_type, names(mlr_reflections$learner_predict_types[[task_type]]))
        assert_subset(properties, mlr_reflections$measure_properties[[task_type]])
      }
      self$properties = properties
      self$predict_type = predict_type
      self$predict_sets = assert_subset(predict_sets, mlr_reflections$predict_sets, empty.ok = FALSE)
      self$task_properties = assert_subset(task_properties, mlr_reflections$task_properties[[task_type]])
      self$packages = assert_set(packages)
      self$man = assert_string(man, na.ok = TRUE)

      check_packages_installed(packages, msg = sprintf("Package '%%s' required but not installed for Measure '%s'", id))
    },

    #' @description
    #' Helper for print outputs.
    format = function() {
      sprintf("<%s:%s>", class(self)[1L], self$id)
    },

    #' @description
    #' Printer.
    #' @param ... (ignored).
    print = function() {
      catf(format(self))
      catf(str_indent("* Packages:", self$packages))
      catf(str_indent("* Range:", sprintf("[%g, %g]", self$range[1L], self$range[2L])))
      catf(str_indent("* Minimize:", self$minimize))
      catf(str_indent("* Properties:", self$properties))
      catf(str_indent("* Predict type:", self$predict_type))
    },

    #' @description
    #' Opens the corresponding help page referenced by field `$man`.
    help = function() {
      open_help(self$man)
    },

    #' @description
    #' Takes a [Prediction] (or a list of [Prediction] objects named with valid `predict_sets`)
    #' and calculates a numeric score.
    #' If the measure if flagged with the properties `"requires_task"`, `"requires_learner"` or `"requires_train_set"`, you must additionally
    #' pass the respective [Task], the trained [Learner] or the training set indices.
    #' This is handled internally during [resample()]/[benchmark()].
    #'
    #' @param prediction ([Prediction] | named list of [Prediction]).
    #'
    #' @param task ([Task]).
    #'
    #' @param learner ([Learner]).
    #'
    #' @param train_set (`integer()`).
    #'
    #' @return `numeric(1)`.
    score = function(prediction, task = NULL, learner = NULL, train_set = NULL) {
      assert_prediction(prediction)

      if ("requires_task" %in% self$properties && is.null(task)) {
        stopf("Measure '%s' requires a task", self$id)
      }

      if ("requires_learner" %in% self$properties && is.null(learner)) {
        stopf("Measure '%s' requires a learner", self$id)
      }

      if ("requires_train_set" %in% self$properties && is.null(train_set)) {
        stopf("Measure '%s' requires the train_set", self$id)
      }

      if (!is_scalar_na(self$task_type) && self$task_type != prediction$task_type) {
        stopf("Measure '%s' incompatible with task type '%s'", self$id, prediction$task_type)
      }

      if (self$predict_type %nin% prediction$predict_types) {
        stopf("Measure '%s' requires predict type '%s'", self$id, self$predict_type)
      }

      measure_score(self, prediction, task, learner, train_set)
    },

    #' @description
    #' Aggregates multiple performance scores into a single score using the `aggregator` function of the measure.
    #' Operates on the [Prediction]s of [ResampleResult] with matching `predict_sets`.
    #'
    #' @param rr [ResampleResult].
    #'
    #' @return `numeric(1)`.
    aggregate = function(rr) {
      if (self$average == "macro") {
        aggregator = self$aggregator %??% mean
        aggregator(measure_score_data(self, rr$data))
      } else { # "micro"
        self$score(rr$prediction(self$predict_sets))
      }
    }
  ),

  active = list(
    #' @template field_hash
    hash = function(rhs) {
      assert_ro_binding(rhs)
      fun = if (exists("score_internal", envir = self, inherits = FALSE)) self$score_internal else private$.score
      hash(class(self), self$id, self$predict_sets, as.character(body(fun)))
    }
  )
)

measure_score = function(measure, prediction, task = NULL, learner = NULL, train_set = NULL) {
  if (is.null(prediction)) {
    return(NaN)
  }

  if (is.list(prediction)) {
    assert_list(prediction, "Prediction", names = "unique")
    ii = match(measure$predict_sets, names(prediction))
    if (anyMissing(ii)) {
      return(NaN)
    }
    prediction = do.call(c, prediction[ii])
  } else {
    assert_prediction(prediction)
  }

  if (exists("score_internal", envir = measure, inherits = FALSE)) {
    # TODO: deprecate this in the future
    measure$score_internal(prediction = prediction, task = task, learner = learner, train_set = train_set)
  } else {
    measure$.__enclos_env__$private$.score(prediction = prediction, task = task, learner = learner, train_set = train_set)
  }
}

measure_score_data = function(measure, data) {
  score = function(prediction, task, learner, resampling, iteration) {
    measure_score(measure, prediction, task, learner, resampling$train_set(iteration))
  }
  pmap_dbl(data[, c("prediction", "task", "learner", "resampling", "iteration"), with = FALSE], score)
}

#' @title Measure Class
#'
#' @usage NULL
#' @format [R6::R6Class] object.
#' @include mlr_reflections.R
#'
#' @description
#' This is the abstract base class for measures like [MeasureClassif] and [MeasureRegr].
#'
#' Measures are classes around tailored around two functions:
#'
#' 1. A function `calculate` which calculates the predictive performance of an [Experiment]
#'    by comparing true and predicted response.
#' 2. A function `aggregator` which combines multiple performance values returned by
#'    `calculate` to a single numeric value.
#'
#' In addition to these two functions, meta-information about the performance measure is stored.
#'
#' Predefined measures are stored in the [Dictionary] [mlr_measures],
#' e.g. [`classif.auc`][mlr_measures_classif.auc] or [`time_train`][mlr_measures_time_train].
#'
#' @section Construction:
#' Note: This object is typically constructed via a derived classes, e.g. [MeasureClassif] or [MeasureRegr].
#'
#' ```
#' m = Measure$new(id, task_type, range, minimize, predict_type = "response",
#'      task_properties = character(0L), na_score = FALSE, packages = character(0L))
#' ```
#'
#' * `id` :: `character(1)`\cr
#'   Identifier for the measure.
#'
#' * `task_type` :: `character(1)`\cr
#'   Type of the task the measure can operator on. E.g., `"classif"` or `"regr"`.
#'
#' * `range` :: `numeric(2)`\cr
#'   Feasible range for this measure as `c(lower_bound, upper_bound)`.
#'   Both bounds may be infinite.
#'
#' * `minimize` :: `logical(1)`\cr
#'   Set to `TRUE` if good predictions correspond to small values,
#'   and to `FALSE` if good predictions correspond to large values.
#'   If set to `NA`, tuning with this measure is not possible.
#'
#' * `aggregator` :: `function(x)`\cr
#'   Function to aggregate individual performance values `x` where `x` is a numeric vector.
#'   If `NULL`, defaults to [base::mean()].
#'
#' * `predict_type` :: `character(1)`\cr
#'   Required predict type of the [Learner].
#'   Possible values are stored in [mlr_reflections$learner_predict_types][mlr_reflections].
#'
#' * `task_properties` :: `character()`\cr
#'   Required task properties, see [Task].
#'
#' * `na_score` :: `logical(1)`\cr
#'   Is the measure expected to return `NA` in some cases? Default is `FALSE`.
#'
#' * `packages` :: `character()`\cr
#'   Set of required packages.
#'   Note that these packages will be loaded via [requireNamespace()], and are not attached.
#'
#'
#' @section Fields:
#' * `id` :: `character(1)`\cr
#'   Identifier of the measure.
#'
#' * `minimize` :: `logical(1)`\cr
#'   Is `TRUE` if the best value is reached via minimization and `FALSE` by maximization.
#'
#' * `packages` :: `character()`\cr
#'   Stores the names of required packages.
#'
#' * `range` :: `numeric(2)`\cr
#'   Stores the feasible range of the measure.
#'
#' * `task_type` :: `character(1)`\cr
#'   Stores the required type of the [Task].
#'
#' * `task_properties` :: `character()`\cr
#'   Stores required properties of the [Task].
#'
#'
#' @section Methods:
#' * `aggregate(rr)`\cr
#'   [ResampleResult] -> `numeric(1)`\cr
#'   Aggregates multiple performance scores into a single score using the `aggregator` function of the measure.
#'   Operates on a [ResampleResult] as returned by [resample].
#'
#' * `calculate(experiment = NULL, prediction = experiment$prediction)`\cr
#'   ([Experiment], [Prediction]) -> `numeric(1)`\cr
#'   Takes an [Experiment] and a [Prediction] (defaults to prediction stored in experiment), and calculates a numeric score.
#'
#' @family Measure
#' @export
Measure = R6Class("Measure",
  cloneable = FALSE,
  public = list(
    id = NULL,
    task_type = NULL,
    predict_type = NULL,
    aggregator = NULL,
    task_properties = NULL,
    range = NULL,
    minimize = NULL,
    na_score = NULL,
    packages = NULL,

    initialize = function(id, task_type, range, minimize = NA, aggregator = NULL, predict_type = "response", task_properties = character(0L), na_score = FALSE, packages = character(0L)) {

      self$id = assert_id(id)
      self$task_type = task_type
      self$range = assert_range(range)
      self$minimize = assert_flag(minimize, na.ok = TRUE)
      if (!is.null(aggregator)) {
        self$aggregator = assert_function(aggregator)
      }

      if (!is_scalar_na(task_type)) {
        assert_choice(task_type, mlr_reflections$task_types)
        assert_choice(predict_type, names(mlr_reflections$learner_predict_types[[task_type]]))
      }
      self$predict_type = predict_type
      self$task_properties = assert_sorted_subset(task_properties, mlr_reflections$task_properties[[task_type]])
      self$na_score = assert_flag(na_score)
      self$packages = assert_set(packages)
    },

    format = function() {
      sprintf("<%s:%s>", class(self)[1L], self$id)
    },

    print = function() {
      catf(format(self))
      catf(str_indent("Packages:", self$packages))
      catf(str_indent("Range:", sprintf("[%g, %g]", self$range[1L], self$range[2L])))
      catf(str_indent("Minimize:", self$minimize))
      catf(str_indent("Predict type:", self$predict_type))
    },

    aggregate = function(rr) {
      fun = self$aggregator %??% mean
      fun(rr$performance(self$id))
    }
  ),

  active = list(
    hash = function() {
      hash(list(class(self), self$id, as.character(body(self$calculate))))
    }
  )
)

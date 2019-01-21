#' @title Measure Class
#'
#' @name Measure
#' @format [R6Class] object.
#' @description
#' Predefined measures are stored in [mlr_measures].
#'
#' @section Usage:
#' ```
#' # Construction
#' m = Measure$new(id, range, minimize, predict_type = "response", task_properties = character(0L), packages = character(0L))
#' m = MeasureClassif$new(id, range, minimize, predict_type = "response", task_properties = character(0L), packages = character(0L))
#' m = MeasureRegr$new(id, range, minimize, predict_type = "response", task_properties = character(0L), packages = character(0L))
#'
#' # Members
#' m$id
#' m$minimize
#' m$packages
#' m$predict_type
#' m$range
#' m$task_properties
#' m$task_type
#'
#' # Methods
#' m$aggregate(rr)
#' m$calculate(e)
#' ```
#'
#' @section Arguments:
#' * `id` (`character(1)`): Identifier for this object.
#' * `range` (`numeric(2)`): Feasible range for this measure as `c(lower_bound, upper_bound)`.
#' * `minimize` (`logical(1)`): Set to `TRUE` if good predictions correspond to small values.
#' * `predict_type` (`character(1)`): Required predict type of the [Learner].
#' * `task_properties` (`character()`): Required task properties, see [Task].
#' * `packages` (`character()`): Set of required packages.
#' * `e` ([Experiment]): Experiment to work on.
#' * `rr` ([ResampleResult]): Performance object returned by [resample] to be aggregated.
#'
#' @section Details:
#' * `$id` (`character(1)`) stores the identifier of the object.
#' * `$minimize` (`logical(1)`) indicates if the good values are reached via minimization.
#' * `$packages` (`character()`) stores the set of required packages.
#' * `$range` (`numeric(2)`) stores the numeric range of feasible measure values.
#' * `$task_properties` (`character()`) defines a set of required task properties.
#' * `$task_type` (`character()`) stores the class names of tasks this measure can operate on.
#'
#' * `$aggregate()` (`function(rr)`) aggregates multiple performance measures using the `aggregate` function. Operates on a [ResampleResult] as returned by [resample].
#' * `$calculate()` (`function(e)`) does the actual work.
#' * `$new()` creates a new object of class [Measure].
#'
#' @family Measure
#' @examples
#' mlr_measures$get("classif.mmce")
NULL

#' @include Mlr3Object.R
#' @export
Measure = R6Class("Measure", inherit = Mlr3Object,
  cloneable = FALSE,
  public = list(
    task_type = NULL,
    predict_type = NULL,
    task_properties = NULL,
    range = NULL,
    minimize = NULL,
    packages = NULL,
    aggregate = function(rr) mean(rr$performance(self$id)),

    initialize = function(id, task_type, range, minimize, predict_type = "response", task_properties = character(0L), packages = character(0L)) {
      super$initialize(id)
      self$task_type = task_type
      self$range = assert_range(range)
      self$minimize = assert_flag(minimize)

      if (!is_scalar_na(task_type)) {
        assert_choice(task_type, mlr_reflections$task_types)
        assert_choice(predict_type, mlr_reflections$predict_types[[task_type]])
      }
      self$predict_type = predict_type
      self$task_properties = assert_subset(task_properties, mlr_reflections$task_properties[[task_type]])
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
    }
  ),

  private = list(
    .calculate_hash = function() {
      hash(list(private$.id, body(self$calculate)))
    }
  )
)

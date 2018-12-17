#' @title Measure Class
#'
#' @description
#' Predefined measures are stored in [mlr_measures].
#'
#' @section Usage:
#' ```
#' # Construction
#' m = Measure$new(id, range, minimize, predict_type = "response", task_properties = character(0L), packages = character(0L))
#' m = MeasureClassif$new(id, range, minimize, predict_type = "response", task_properties = character(0L), packages = character(0L))
#' m = MeasureRegr$new(id, range, minimize, predict_type = "response", task_properties = character(0L), packages = character(0L))
#' #
#' m$id
#' m$packages
#' m$task_type
#' m$task_properties
#' m$calculate(e)
#' m$range
#' m$minimize
#' m$aggregate(rr)
#' ```
#'
#' @section Arguments:
#' * `id` (`character(1)`):\cr
#'   Identifier for this object.
#' * `range` (`numeric(2)`):\cr
#'   Feasible range for this measure as `c(lower_bound, upper_bound)`.
#' * `minimize` (`logical(1)`)\cr:
#'   Set to `TRUE` if good predictions correspond to small values.
#' * `predict_type` (`character(1)`):\cr
#'   Required predict type of the [Learner].
#' * `task_properties` (`character()`):\cr
#'   Required task properties, see [Task].
#' * `packages` (`character()`):\cr
#'   Set of required packages.
#' * `e` ([Experiment]):\cr
#'   Experiment to work on.
#' * `rr` ([ResampleResult]):\cr
#'   Performance object returned by [resample] to be aggregated.
#'
#' @section Details:
#' * `$new()` creates a new object of class [Measure].
#'
#' * `$id` (`character(1)`) stores the identifier of the object.
#'
#' * `$packages` (`character()`) stores the set of required packages.
#'
#' * `$task_type` (`character()`) stores the class names of tasks this measure can operate on.
#'
#' * `$task_properties` (`character()`) defines a set of required task properties.
#'
#' * `$range` (`numeric(2)`) stores the numeric range of feasible measure values.
#'
#' * `$minimize` (`logical(1)`) indicates if the good values are reached via minimization.
#'
#' * `$calculate` (`function(e)`) does the actual work.
#'
#' * `$aggregate` (`function(rr)`) aggregates multiple performance measures using the `aggregate` function.
#'   Operates on a [ResampleResult] as returned by [resample].
#'
#' @name Measure
#' @family Measure
#' @examples
#' mlr_measures$get("mmce")
NULL

#' @export
Measure = R6Class("Measure", cloneable = FALSE,
  public = list(
    id = NULL,
    task_type = NULL,
    predict_type = NULL,
    task_properties = NULL,
    range = NULL,
    minimize = NULL,
    packages = NULL,
    aggregate = function(rr) mean(rr$performance(self$id)),

    initialize = function(id, task_type, range, minimize, predict_type = "response", task_properties = character(0L), packages = character(0L)) {
      self$id = assert_id(id)
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

    print = function() {
      catf("<%s> for %s", class(self)[1L], self$task_type)
      catf(str_indent("Packages:", self$packages))
      catf(str_indent("Range:", sprintf("[%g, %g]", self$range[1L], self$range[2L])))
      catf(str_indent("Minimize:", self$minimize))
      catf(str_indent("Predict type:", self$predict_type))
    },

    calculate = function(...) stopf("Method not implemented, should have been overloaded during construction")
  )
)

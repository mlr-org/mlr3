#' @title Abstract measure class
#'
#' @description
#' Abstraction for performance measures.
#'
#' Predefined measures are stored in [mlr_measures].
#'
#' @section Usage:
#' ```
#' m = Measure$new(id)
#' m$id
#' m$packages
#' m$task_type
#' m$task_properties
#' m$learner_properties
#' m$calculate(e)
#' m$range
#' m$minimize
#' m$aggregate(rr)
#' ```
#'
#' @section Arguments:
#' * `id` (`character(1)`):
#'   identifier for this object.
#' * `e` ([Experiment]):
#'   Experiment to work on.
#' * `rr` ([ResampleResult]):
#'   Performance object returned by [resample] to be aggregated.
#'
#' @section Details:
#' `$new()` creates a new object of class [Measure].
#'
#' `$id` (`character(1)`) stores the identifier of the object.
#'
#' `$packages` (`character(1)`) stores the names of required packages.
#'
#' `$task_type` (`character`) stores the class names of tasks this measure can operate on.
#'
#' `$task_properties` (`character`) defines a set of required task properties.
#'
#' `$learner_properties` (`character`) defines a set of required learner properties.
#'
#' `$range` (`numeric(2)`) stores the numeric range of feasible measure values.
#'
#' `$minimize` (`logical(1)`) indicates if the good values are reached via minimization.
#'
#' `$calculate` (`function`) does the actual work.
#'
#' `$aggregate` (`function`) aggregates multiple performance measures using the `aggregate` function.
#'   Operates on a [ResampleResult] as returned by [resample].
#'
#' @name Measure
#' @keywords internal
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
    learner_properties = NULL,
    range = NULL,
    minimize = NULL,
    packages = NULL,
    aggregate = function(rr) mean(rr$performance[[self$id]]),

    initialize = function(id, task_type, range, minimize, predict_type = "response", task_properties = character(0L), learner_properties = character(0L), packages = character(0L)) {
      self$id = assert_id(id)
      self$task_type = task_type
      self$range = assert_range(range)
      self$minimize = assert_flag(minimize)

      if (!is_scalar_na(task_type)) {
        assert_choice(task_type, capabilities$task_types)
        assert_choice(predict_type, capabilities$predict_types[[task_type]])
      }
      self$predict_type = predict_type
      self$task_properties = assert_subset(task_properties, capabilities$task_properties[[task_type]])
      self$learner_properties = assert_subset(learner_properties, capabilities$learner_properties[[task_type]])
      self$packages = assert_set(packages)
    },

    calculate = function(...) stopf("Method not implemented, should have been overloaded during construction")
  )
)

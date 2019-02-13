#' @title Measure Class
#'
#' @usage NULL
#' @format [R6::R6Class] object.
#'
#' @description
#' This is the abstract base class for measures like [MeasureClassif] and [MeasureRegr].
#' Predefined measures are stored in [mlr_measures].
#'
#' @section Construction:
#' ```
#' m = Measure$new(id, task_type, range, minimize, predict_type = "response",
#'      task_properties = character(0L), packages = character(0L))
#' ```
#'
#' * `id` :: `character(1)`\cr
#'   Identifier for the measure.
#'
#' * `task_type` :: `character(1)`\\cr
#'   Type of the task the measure can operator on. E.g., `\"classif\"` or `\"regr\"`.
#'
#' * `range` :: `numeric(2)`\cr
#'   Feasible range for this measure as `c(lower_bound, upper_bound)`.
#'
#' * `minimize` :: `logical(1)`\cr
#'   Set to `TRUE` if good predictions correspond to small values.
#'
#' * `predict_type` :: `character(1)`\cr
#'   Required predict type of the [Learner].
#'
#' * `task_properties` :: `character()`\cr
#'   Required task properties, see [Task].
#'
#' * `packages` :: `character()`\cr
#'   Set of required packages.
#'   Note that these packages will be loaded via [requireNamespace()], and are not attached.
#'
#'
#' @section Fields:
#' * `id` :: `character(1)`\cr
#'   Stores the identifier of the measure.
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
#'   Aggregates multiple performance scores into a single score using the `aggregate` function of the measure.
#'   Operates on a [ResampleResult] as returned by [resample].
#'
#' * `calculate(e)`\cr
#'   [Experiment] -> `numeric(1)`\cr
#'   Takes an [Experiment], extracts the predictions (as well as other possibly needed objects), and calculates
#'   a score.
#'
#' @family Measure
#' @export
Measure = R6Class("Measure",
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
      self$id = assert_id(id)
      self$task_type = task_type
      self$range = assert_range(range)
      self$minimize = assert_flag(minimize)

      if (!is_scalar_na(task_type)) {
        assert_choice(task_type, mlr_reflections$task_types)
        assert_choice(predict_type, mlr_reflections$predict_types[[task_type]])
      }
      self$predict_type = predict_type
      self$task_properties = assert_sorted_subset(task_properties, mlr_reflections$task_properties[[task_type]])
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
      hash(list(class(self), private$.id, as.character(body(self$calculate))))
    }
  )
)

add_id_hash(Measure)

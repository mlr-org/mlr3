#' @title Regression Measure
#'
#' @usage NULL
#' @format [R6::R6Class] object inheriting from [Measure].
#' @include Measure.R
#'
#' @description
#' This measure specializes [Measure] for regression problems.
#' Predefined measures can be found in the [Dictionary] [mlr_measures].
#'
#' The `task_type` is set to `"regr"`.
#'
#' @section Construction:
#' ```
#' m = MeasureRegr$new(id, range, minimize, predict_type = "response",
#'      task_properties = character(0L), packages = character(0L))
#' ```
#'
#' * `id` :: `character(1)`\cr
#'   Identifier for the measure.
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
#' * `na_score` :: `logical(1)`\cr
#'   Is the measure expected to return `NA` in some edge cases?
#'   Default is `FALSE`.
#'
#' * `packages` :: `character()`\cr
#'   Set of required packages.
#'   Note that these packages will be loaded via [requireNamespace()], and are not attached.
#'
#' @section Fields:
#' @inheritSection Measure Fields
#'
#' @section Methods:
#' @inheritSection Measure Methods
#'
#' @family Measure
#' @export
MeasureRegr = R6Class("MeasureRegr", inherit = Measure, cloneable = FALSE,
  public = list(
    initialize = function(id, range, minimize, predict_type = "response", task_properties = character(0L), na_score = FALSE, packages = character(0L)) {
      super$initialize(id, task_type = "regr", range = range, minimize = minimize, predict_type = predict_type,
        task_properties = task_properties, na_score = na_score, packages = packages)
    }
  )
)

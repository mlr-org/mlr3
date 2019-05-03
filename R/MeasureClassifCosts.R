#' @title Cost-sensitive Classification Measure
#'
#' @name mlr_measures_classif.costs
#' @format [R6::R6Class()] inheriting from [MeasureClassif].
#' @include MeasureClassif.R
#'
#' @description
#' Uses a cost matrix to create a classification measure.
#' The cost matrix is stored as slot "costs".
#' Costs are aggregated with the mean.
#'
#' @section Construction:
#' ```
#' MeasureClassifCosts$new(costs = NULL, normalize = TRUE)
#' ```
#'
#' * `id` :: `character(1)`\cr
#'   Identifier for the measure.
#'
#' * `costs` :: `matrix()`\cr
#'   Numeric matrix of costs (truth in columns, predicted response in rows).
#' * `normalize` :: `logical(1)`\cr
#'   If `TRUE`, calculate the mean costs instead of the total costs.
#'
#' @export
MeasureClassifCosts = R6Class("MeasureClassifCosts",
  inherit = MeasureClassif,
  cloneable = FALSE,
  public = list(
    normalize = NULL,

    initialize = function(id = "classif.costs", costs = NULL, normalize = TRUE) {
      super$initialize(
        id = id,
        range = c(-Inf, Inf),
        minimize = TRUE
      )

      if (!is.null(costs))
        self$costs = costs
      self$normalize = assert_flag(normalize)
    },

    calculate = function(experiment = NULL, prediction = experiment$prediction) {
      costs = assert_cost_matrix(private$.costs, experiment$task)
      confusion = prediction$confusion
      ii = match(rownames(confusion), rownames(costs))
      jj = match(colnames(confusion), colnames(costs))
      if (is.unsorted(ii) || is.unsorted(jj))
        confusion = confusion[ii, jj]
      score = sum(confusion * costs)
      if (self$normalize)
        score = score / sum(confusion)
      score
    }
  ),

  active = list(
    costs = function(rhs) {
      if (missing(rhs))
        return(private$.costs)
      private$.costs = assert_cost_matrix(rhs)

      if (min(rhs) >= 0)
        self$range[1L] = 0
      if (max(rhs) <= 0)
        self$range[2L] = 0
    }
  ),

  private = list(
    .costs = NULL
  )
)

assert_cost_matrix = function(costs, task = NULL) {
  if (is.null(task)) {
    assert_matrix(costs, mode = "numeric", any.missing = FALSE, col.names = "unique", row.names = "unique")
  } else {
    lvls = task$class_names
    assert_matrix(costs, mode = "numeric", any.missing = FALSE, nrows = length(lvls), ncols = length(lvls), row.names = "unique", col.names = "unique")
    assert_names(colnames(costs), permutation.of = lvls)
    assert_names(rownames(costs), permutation.of = lvls)
  }

  costs
}

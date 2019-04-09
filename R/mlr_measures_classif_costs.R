#' @title Cost-sensitive Classification Measure
#'
#' @name mlr_measures_classif.costs
#' @format [R6::R6Class()] inheriting from [MeasureClassif].
#' @include MeasureClassif.R
#'
#' @description
#' The cost matrix is stored as slot "costs" and must be set manually.
#'
#' @export
MeasureClassifCosts = R6Class("MeasureClassifCosts",
  inherit = MeasureClassif,
  cloneable = FALSE,
  public = list(
    initialize = function(costs = NULL) {
      super$initialize(
        id = "classif.costs",
        range = c(-Inf, Inf),
        minimize = TRUE
      )

      if (!is.null(costs))
        self$costs = costs
    },

    calculate = function(e) {
      costs = assert_cost_matrix(private$.costs, e$task)
      confusion = e$prediction$confusion
      ii = match(rownames(confusion), rownames(costs))
      jj = match(colnames(confusion), colnames(costs))
      if (is.unsorted(ii) || is.unsorted(jj))
        confusion = confusion[ii, jj]
      sum(confusion * costs)
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

#' @include mlr_measures.R
mlr_measures$add("classif.costs", MeasureClassifCosts)


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

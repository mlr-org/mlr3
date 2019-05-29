#' @title Cost-sensitive Classification Measure
#'
#' @aliases mlr_measures_classif.costs
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
#'
#' * `normalize` :: `logical(1)`\cr
#'   If `TRUE`, calculate the mean costs instead of the total costs.
#'
#' @export
#' @examples
#' # get a cost sensitive task
#' task = mlr_tasks$get("german_credit")
#'
#' # cost matrix as given on the UCI page of the german credit data set
#' # https://archive.ics.uci.edu/ml/datasets/statlog+(german+credit+data)
#' costs = matrix(c(0, 5, 1, 0), nrow = 2)
#' dimnames(costs) = list(truth = task$class_names, predicted = task$class_names)
#' print(costs)
#'
#' # mlr3 needs truth in columns, predictions in rows
#' costs = t(costs)
#'
#' # create measure which calculates the absolute costs
#' m = MeasureClassifCosts$new(id = "german_credit_costs", costs, normalize = FALSE)
#'
#' # fit models and calculate the costs
#' resample(task, "classif.rpart", "cv3", measure = m)
MeasureClassifCosts = R6Class("MeasureClassifCosts",
  inherit = MeasureClassif,
  public = list(
    normalize = NULL,

    initialize = function(id = "classif.costs", costs = NULL, normalize = TRUE) {
      super$initialize(
        id = id,
        range = c(-Inf, Inf),
        minimize = TRUE
      )

      if (!is.null(costs)) {
        self$costs = costs
      }
      self$normalize = assert_flag(normalize)
    },

    calculate = function(experiment = NULL, prediction = experiment$prediction) {

      costs = assert_cost_matrix(private$.costs, experiment$task)
      confusion = prediction$confusion

      # reorder rows / cols if necessary
      ii = match(rownames(confusion), rownames(costs))
      jj = match(colnames(confusion), colnames(costs))
      if (is.unsorted(ii) || is.unsorted(jj)) {
        confusion = confusion[ii, jj]
      }

      perf = sum(confusion * costs)
      if (self$normalize) {
        perf = perf / sum(confusion)
      }
      perf
    }
  ),

  active = list(
    costs = function(rhs) {
      if (missing(rhs)) {
        return(private$.costs)
      }
      private$.costs = assert_cost_matrix(rhs)

      if (min(rhs) >= 0) {
        self$range[1L] = 0
      }
      if (max(rhs) <= 0) {
        self$range[2L] = 0
      }
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

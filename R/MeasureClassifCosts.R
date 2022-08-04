#' @title Cost-sensitive Classification Measure
#'
#' @name mlr_measures_classif.costs
#' @include MeasureClassif.R
#'
#' @description
#' Uses a cost matrix to create a classification measure.
#' True labels must be arranged in columns, predicted labels must be arranged  in rows.
#' The cost matrix is stored as slot `$costs`.
#'
#' For calculation of the score, the confusion matrix is multiplied element-wise with the cost matrix.
#' The costs are then summed up (and potentially divided by the number of observations if `normalize` is set to `TRUE` (default)).
#'
#' This measure requires the [Task] during scoring to ensure that the rows and columns of the cost matrix are in the same order as in the confusion matrix.
#'
#' @templateVar id classif.costs
#' @template measure
#'
#' @template seealso_measure
#' @family classification measures
#' @family multiclass classification measures
#' @export
#' @examples
#' # get a cost sensitive task
#' task = tsk("german_credit")
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
#' # create a cost measure which calculates the absolute costs
#' m = msr("classif.costs", id = "german_credit_costs", costs = costs, normalize = FALSE)
#'
#' # fit models and evaluate with the cost measure
#' learner = lrn("classif.rpart")
#' rr = resample(task, learner, rsmp("cv", folds = 3))
#' rr$aggregate(m)
MeasureClassifCosts = R6Class("MeasureClassifCosts",
  inherit = MeasureClassif,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      param_set = ps(normalize = p_lgl(default = TRUE, tags = "required"))
      param_set$values = list(normalize = TRUE)

      super$initialize(
        id = "classif.costs",
        param_set = param_set,
        properties = "requires_task",
        range = c(-Inf, Inf),
        minimize = TRUE,
        label = "Cost-sensitive Classification",
        man = "mlr3::mlr_measures_classif.costs"
      )
    }
  ),

  active = list(
    #' @field costs (numeric `matrix()`)\cr
    #' Matrix of costs (truth in columns, predicted response in rows).
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
    .costs = NULL,

    .score = function(prediction, task, ...) {
      costs = assert_cost_matrix(private$.costs, task)
      confusion = table(response = prediction$response, truth = prediction$truth, useNA = "ifany")

      # reorder rows / cols if necessary
      ii = reorder_vector(rownames(confusion), rownames(costs))
      jj = reorder_vector(colnames(confusion), colnames(costs))
      if (is.unsorted(ii) || is.unsorted(jj)) {
        confusion = confusion[ii, jj]
      }

      perf = sum(confusion * costs)
      if (self$param_set$values$normalize) {
        perf = perf / sum(confusion)
      }
      perf
    },

    .extra_hash = "costs"
  )
)

#' @include mlr_measures.R
mlr_measures$add("classif.costs", function() MeasureClassifCosts$new())

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

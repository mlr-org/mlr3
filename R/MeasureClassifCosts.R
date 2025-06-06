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
      param_set = ps(normalize = p_lgl(tags = "required"))
      param_set$set_values(normalize = TRUE)

      super$initialize(
        id = "classif.costs",
        param_set = param_set,
        range = c(-Inf, Inf),
        minimize = TRUE,
        properties = "weights",
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

      assert_matrix(rhs, mode = "numeric", any.missing = FALSE, col.names = "unique", row.names = "unique")
      assert_set_equal(rownames(rhs), colnames(rhs))
      private$.costs = rhs

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

    .score = function(prediction, weights, ...) {
      costs = self$costs
      lvls = levels(prediction$truth)
      assert_set_equal(lvls, colnames(costs))

      if (is.null(weights)) {
        confusion = table(response = prediction$response, truth = prediction$truth, useNA = "ifany")
      } else {
        confusion = tapply(weights,
          list(response = addNA(prediction$response, ifany = TRUE), truth = addNA(prediction$truth, ifany = TRUE)),
          sum, default = 0
        )
      }

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

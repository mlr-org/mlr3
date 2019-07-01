#' @title Classification Learner
#'
#' @usage NULL
#' @format [R6::R6Class] object inheriting from [Learner].
#' @include Learner.R
#'
#' @description
#' This Learner specializes [Learner] for classification problems.
#'
#' Predefined learners can be found in the [Dictionary] [mlr_learners].
#'
#' @section Construction:
#' ```
#' l = LearnerClassif$new(id, param_set = ParamSet$new(), param_vals = list(), predict_types = character(),
#'      feature_types = character(), properties = character(), data_formats = "data.table", packages = character())
#' ```
#' For a description of the arguments, see [Learner].
#' `task_type` is set to `"classif"`.
#' Possible values for `predict_types` are a subset of `c("response", "prob")`.
#'
#' @section Fields:
#' See [Learner].
#'
#' @section Methods:
#' All methods of [Learner], and additionally:
#'
#' * `new_prediction(row_ids, truth, response = NULL, prob = NULL)`\cr
#'   (`integer()` | `character()`, `factor()`, `factor()`, `matrix()`) -> [PredictionClassif]\cr
#'   Creates a new [PredictionClassif] object, after performing some basic type checks and transformations.
#'   See [PredictionClassif] for a description of the arguments.
#'
#' @family Learner
#' @seealso Example classification learner: [`classif.rpart`][mlr_learners_classif.rpart].
#' @export
#' @examples
#' # get all classification learners from mlr_learners:
#' lrns = mlr_learners$mget(mlr_learners$keys("^classif"))
#' names(lrns)
#'
#' # get a specific learner from mlr_learners:
#' lrn = mlr_learners$get("classif.rpart")
#' print(lrn)
LearnerClassif = R6Class("LearnerClassif", inherit = Learner,
  public = list(
    initialize = function(id, param_set = ParamSet$new(), param_vals = list(), predict_types = "response", feature_types = character(), properties = character(), data_formats = "data.table", packages = character()) {
      super$initialize(id = id, task_type = "classif", param_set = param_set, param_vals = param_vals,
        predict_types = predict_types, feature_types = feature_types, properties = properties,
        data_formats = data_formats, packages = packages)
    },

    new_prediction = function(row_ids, truth, response = NULL, prob = NULL) {
      row_ids = assert_row_ids(row_ids)
      n = length(row_ids)
      assert_factor(truth, len = n)
      lvls = levels(truth)

      if (!is.null(response)) {
        response = as_factor(response, levels = lvls)
        assert_factor(response, len = n)
      }

      if (!is.null(prob)) {
        assert_matrix(prob, nrows = n, ncols = length(lvls))
        assert_numeric(prob, lower = 0, upper = 1)
        assert_names(colnames(prob), permutation.of = lvls)
        if (!is.null(rownames(prob))) {
          rownames(prob) = NULL
        }

        if (is.null(response)) {
          # calculate response from prob
          i = max.col(prob, ties.method = "random")
          response = factor(colnames(prob)[i], levels = lvls)
        }
      }

      PredictionClassif$new(row_ids = row_ids, truth = truth, response = response, prob = prob)
    }
  )
)

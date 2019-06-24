#' @title Regression Learner
#'
#' @usage NULL
#' @format [R6::R6Class] object inheriting from [Learner].
#' @include Learner.R
#'
#' @description
#' This Learner specializes [Learner] for regression problems.
#'
#' Predefined learners can be found in the [Dictionary] [mlr_learners].
#'
#' @section Construction:
#' ```
#' l = LearnerRegr$new(id, param_set = ParamSet$new(), param_vals = list(), predict_types = character(),
#'      feature_types = character(), properties = character(), data_formats = "data.table", packages = character())
#' ```
#' For a description of the arguments, see [Learner].
#' `task_type` is set to `"regr"`.
#' Possible values for `predict_types` are a subset of `c("response", "se")`.
#'
#' @section Fields:
#' See [Learner].
#'
#' @section Methods:
#' All methods of [Learner], and additionally:
#'
#' * `new_prediction(task, response = NULL, prob = NULL)`\cr
#'   ([Task], `numeric()`, `numeric()`) -> [PredictionRegr]\cr
#'   This method is intended to be called in `predict()` to create a [PredictionRegr] object.
#'   Uses `task` to extract `row_ids`.
#'   To manually construct a [PredictionRegr] object, see its constructor.
#'
#' @family Learner
#' @seealso Example regression learner: [`regr.rpart`][mlr_learners_regr.rpart].
#' @export
#' @examples
#' # get all regression learners from mlr_learners:
#' lrns = mlr_learners$mget(mlr_learners$keys("^regr"))
#' names(lrns)
#'
#' # get a specific learner from mlr_learners:
#' lrn = mlr_learners$get("regr.rpart")
#' print(lrn)
LearnerRegr = R6Class("LearnerRegr", inherit = Learner,
  public = list(
    initialize = function(id, param_set = ParamSet$new(), param_vals = list(), predict_types = "response", feature_types = character(), properties = character(), data_formats = "data.table", packages = character()) {
      super$initialize(id = id, task_type = "regr", param_set = param_set, param_vals = param_vals,
        feature_types = feature_types, predict_types = predict_types, properties = properties,
        data_formats = data_formats, packages = packages)
    },

    new_prediction = function(task, response = NULL, se = NULL) {
      row_ids = task$row_ids
      n = length(row_ids)
      assert_numeric(response, len = n, any.missing = FALSE, null.ok = TRUE)
      assert_numeric(se, len = n, lower = 0, any.missing = FALSE, null.ok = TRUE)

      PredictionRegr$new(row_ids = row_ids, truth = task$truth(), response = response, se = se)
    }
  )
)

#' @title Dictionary of Learners
#'
#' @usage NULL
#' @format [R6::R6Class] object inheriting from [mlr3misc::Dictionary].
#' @description
#' A simple [Dictionary] storing objects of class [Learner].
#' Each learner has an associated help page, see `mlr_learners_[id]`.
#'
#' @section Methods:
#' See [Dictionary].
#'
#' @section S3 methods:
#' * `as.data.table(dict)`\cr
#'   [Dictionary] -> [data.table::data.table()]\cr
#'   Returns a [data.table::data.table()] with fields "key", "feature_types", "packages",
#'   "properties" and "predict_types" as columns.
#'
#' @family Dictionary
#' @family Learner
#' @seealso Example learners:
#' * [`classif.rpart`][mlr_learners_classif.rpart]
#' * [`regr.rpart`][mlr_learners_regr.rpart]
#' * [`classif.featureless`][mlr_learners_classif.featureless]
#' * [`regr.featureless`][mlr_learners_regr.featureless]
#' * [`classif.debug`][mlr_learners_classif.debug]
#' @export
#' @examples
#' as.data.table(mlr_learners)
#' mlr_learners$get("classif.featureless")
mlr_learners = R6Class("DictionaryLearner",
  inherit = Dictionary,
  cloneable = FALSE,

  public = list(
    get = function(key, id = NULL, param_vals = NULL, predict_type = NULL) {
      obj = super$get(key)
      if (!is.null(id)) {
        obj$id = id
      }
      if (!is.null(param_vals)) {
        obj$param_set$values = insert_named(obj$param_set$values, param_vals)
      }
      if (!is.null(predict_type)) {
        obj$predict_type = predict_type
      }
      obj
    }
  )
)$new()

#' @export
as.data.table.DictionaryLearner = function(x, ...) {
  setkeyv(map_dtr(x$keys(), function(key) {
    l = x$get(key)
    list(
      key = key,
      feature_types = list(l$feature_types),
      packages = list(l$packages),
      properties = list(l$properties),
      predict_types = list(l$predict_types)
    )
  }), "key")[]
}

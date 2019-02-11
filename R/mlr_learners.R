#' @title Dictionary of Learners
#'
#' @format [R6::R6Class] object
#' @description
#' A simple [Dictionary] storing objects of class [Learner].
#' Each learner has an associated help page, see `mlr_learners_[id]`.
#'
#' @section Usage:
#'
#' See [Dictionary].
#'
#' @family Dictionary
#' @family Learner
#' @name mlr_learners
#' @examples
#' as.data.table(mlr_learners)
#' mlr_learners$get("classif.featureless")
NULL

#' @include Dictionary.R
DictionaryLearner = R6Class("DictionaryLearner",
  inherit = Dictionary,
  cloneable = FALSE,

  public = list(
    get = function(key, id = NULL, param_vals = NULL, predict_type = NULL) {
      obj = dictionary_retrieve(self, key)
      if (!is.null(id))
        obj$id = id
      if (!is.null(param_vals))
        obj$param_set$values = insert_named(obj$param_set$values, param_vals)
      if (!is.null(predict_type))
        obj$predict_type = predict_type
      obj
    }
  )
)

#' @export
mlr_learners = DictionaryLearner$new()

#' @export
as.data.table.DictionaryLearner = function(x, ...) {
  setkeyv(map_dtr(x$keys(), function(key) {
    l = x$get(key)
    list(
      id = key,
      feature_types = list(l$feature_types),
      packages = list(l$packages),
      properties = list(l$properties),
      predict_types = list(l$predict_types)
    )
  }), "id")[]
}

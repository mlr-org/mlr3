#' @title Dictionary of Learners
#'
#' @format [R6Class] object
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
#' mlr_learners$ids()
#' as.data.table(mlr_learners)
#' mlr_learners$get("classif.featureless")
NULL

#' @include Dictionary.R
DictionaryLearner = R6Class("DictionaryLearner",
  inherit = Dictionary,
  cloneable = FALSE,

  public = list(
    get = function(key, param_vals = list(), ...) {
      assert_ids_exist(assert_id(key), self)
      obj = dictionary_retrieve(self, key, ...)
      if (length(param_vals))
        obj$param_set$values = insert_named(obj$param_set$values, param_vals)
      obj
    }
  )
)

#' @export
mlr_learners = DictionaryLearner$new()

#' @export
as.data.table.DictionaryLearner = function(x, ...) {
  setkeyv(map_dtr(x$ids(), function(id) {
    l = x$get(id)
    list(
      id = id,
      feature_types = list(l$feature_types),
      packages = list(l$packages),
      properties = list(l$properties),
      predict_types = list(l$predict_types)
    )
  }), "id")[]
}

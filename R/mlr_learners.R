#' @title Dictionary of Learners
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
#' @references [HTML help page](https://mlr3.mlr-org.com/reference/mlr_learners.html)
#' @examples
#' mlr_learners$ids()
#' as.data.table(mlr_learners)
#' mlr_learners$get("classif.featureless")
NULL

#' @include Dictionary.R
DictionaryLearner = R6Class("DictionaryLearner",
  inherit = Dictionary,
  cloneable = FALSE
)

#' @export
mlr_learners = DictionaryLearner$new()

#' @export
as.data.table.DictionaryLearner = function(x, ...) {
  setkeyv(map_dtr(x$ids(), function(id) {
    l = x$get(id)
    list(id = id, packages = list(l$packages))
  }), "id")[]
}

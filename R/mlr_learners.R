#' @title Dictionary of registered learners
#'
#' @section Usage:
#' See [Dictionary].
#'
#' @family Dictionary
#' @family Learner
#' @name mlr_learners
#' @examples
#' mlr_learners$keys()
#' as.data.table(mlr_learners)
#' l = mlr_learners$get("classif.dummy")
#' print(l)
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
  setkeyv(rbindlist(lapply(x$ids, function(id) {
    l = x$get(id)
    data.table(id = id, packages = list(l$packages))
  })), "id")[]
}

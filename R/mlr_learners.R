#' @title Registered Learners
#' @docType class
#' @format \code{\link{R6Class}} object
#'
#' @description
#' \code{mlr_learners} is a \code{\link{Dictionary}} used to manage learners.
#'
#' @include Dictionary.R
#' @export
#' @examples
#' mlr_learners$ids
#' mlr_learners$get("classif.dummy")
mlr_learners = Dictionary$new("Learner")
class(mlr_learners) = c("DictionaryLearners", class(mlr_learners))


#' @export
as.data.table.DictionaryLearners = function(x, ...) {
  rbindlist(eapply(x$items, function(obj) {
    list(task_type = obj$task_type, name = obj$name, properties = list(obj$properties), packages = list(obj$packages))
  }))
}


#' @export
as.data.frame.DictionaryLearners = function(x, ...) {
  setDF(as.data.table(x))[]
}

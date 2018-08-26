#' @title Registered Learners
#' @docType class
#' @format \code{\link{R6Class}} object
#'
#' @description
#' \code{mlr.learners} is a \code{\link{Dictionary}} used to manage learners.
#'
#' @include Dictionary.R
#' @export
#' @examples
#' mlr.learners$ids
#' mlr.learners$get("classif.dummy")
mlr.learners = Dictionary$new("Learner")
class(mlr.learners) = c("DictionaryLearners", class(mlr.learners))


#' @export
as.data.table.DictionaryLearners = function(x, ...) {
  rbindlist(eapply(x$items, function(obj) {
    list(task.type = obj$task.type, name = obj$name, properties = list(obj$properties), packages = list(obj$packages))
  }))
}


#' @export
as.data.frame.DictionaryLearners = function(x, ...) {
  setDF(as.data.table(x))[]
}

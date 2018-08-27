#' @title Registered Learners
#' @docType class
#' @format [R6Class()] object
#'
#' @description
#' `mlr_learners` is a [Dictionary()] used to manage learners.
#'
#' @export
#' @examples
#' mlr_learners$ids
#' mlr_learners$get("classif.dummy")
mlr_learners = Dictionary$new("Learner")

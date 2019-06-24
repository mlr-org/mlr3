#' @title German Credit Classification Task
#'
#' @name mlr_tasks_german_credit
#' @format [R6::R6Class] inheriting from [TaskClassif].
#' @include mlr_tasks.R
#'
#' @section Usage:
#' ```
#' mlr_tasks$get("german_credit")
#' ```
#'
#' @description
#' A classification task for the German credit data set.
#' The aim is to predict creditworthiness, labeled as "good" and "bad".
#' Positive class is set to label "good".
#'
#' The default performance measure uses [MeasureClassifCosts]: Correct classifications
#' have zero cost, while incorrectly predicting "good" yields a cost of 5 incorrectly
#' predicting "bad" yields a cost of 1.
#'
#' @source
#' Data set originally published on [UCI](https://archive.ics.uci.edu/ml/datasets/statlog+(german+credit+data)).
#' This is the preprocessed version taken from package \CRANpkg{evtree}.
#'
#' Donor:
#' Professor Dr. Hans Hofmann\cr
#' Institut für Statistik und Ökonometrie\cr
#' Universität Hamburg\cr
#' FB Wirtschaftswissenschaften\cr
#' Von-Melle-Park 5\cr
#' 2000 Hamburg 13
NULL

load_task_german_credit = function(id = "german_credit") {
  b = as_data_backend(readRDS(system.file("extdata", "german_credit.rds", package = "mlr3")))
  b$hash = "_mlr3_tasks_german_credit_"
  task = TaskClassif$new(id, b, target = "credit_risk", positive = "good")
  costs = matrix(c(0, 1, 5, 0), nrow = 2)
  dimnames(costs) = list(predicted = task$class_names, truth = task$class_names)
  task$measures = list(MeasureClassifCosts$new("german_credit_costs", costs))
  task
}

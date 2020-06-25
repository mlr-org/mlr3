#' @title German Credit Classification Task
#'
#' @name mlr_tasks_german_credit
#' @format [R6::R6Class] inheriting from [TaskClassif].
#' @include mlr_tasks.R
#'
#' @section Construction:
#' ```
#' mlr_tasks$get("german_credit")
#' tsk("german_credit")
#' ```
#'
#' @description
#' A classification task for the German credit data set.
#' The aim is to predict creditworthiness, labeled as "good" and "bad".
#' Positive class is set to label "good".
#'
#' See example for the creation of a [MeasureClassifCosts] as described misclassification costs.
#'
#' @source
#' Data set originally published on [UCI](https://archive.ics.uci.edu/ml/datasets/statlog+(german+credit+data)).
#' This is the preprocessed version taken from package \CRANpkg{rchallenge} with
#' factors instead of dummy variables, and corrected as proposed by Ulrike
#' Grömping.
#'
#' Donor:
#' Professor Dr. Hans Hofmann\cr
#' Institut für Statistik und Ökonometrie\cr
#' Universität Hamburg\cr
#' FB Wirtschaftswissenschaften\cr
#' Von-Melle-Park 5\cr
#' 2000 Hamburg 13
#'
#' @references
#' \cite{mlr3}{gromping_2019}
#'
#' @template seealso_task
#' @examples
#' task = tsk("german_credit")
#' costs = matrix(c(0, 1, 5, 0), nrow = 2)
#' dimnames(costs) = list(predicted = task$class_names, truth = task$class_names)
#' measure = msr("classif.costs", id = "german_credit_costs", costs = costs)
#' print(measure)
NULL

load_task_german_credit = function(id = "german_credit") {
  b = as_data_backend(readRDS(system.file("extdata", "german_credit.rds", package = "mlr3")))
  task = TaskClassif$new(id, b, target = "credit_risk", positive = "good")
  b$hash = task$man = "mlr3::mlr_tasks_german_credit"
  task
}

#' @include mlr_tasks.R
mlr_tasks$add("german_credit", load_task_german_credit)

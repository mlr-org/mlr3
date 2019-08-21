#' @title Wine Classification Task
#'
#' @name mlr_tasks_wine
#' @format [R6::R6Class] inheriting from [TaskClassif].
#' @include mlr_tasks.R
#'
#' @section Usage:
#' ```
#' mlr_tasks$get("wine")
#' tsk("wine")
#' ```
#'
#' @description
#' Wine data set from the UCI machine learning repository (\url{https://archive.ics.uci.edu/ml/datasets/wine}).
#' Results of a chemical analysis of three types of wines grown in the same region in Italy but derived from three different cultivars.
#'
#' @source
#' Original owners:
#' Forina, M. et al, PARVUS - An Extendible Package for Data Exploration, Classification and Correlation.
#' Institute of Pharmaceutical and Food Analysis and Technologies, Via Brigata Salerno, 16147 Genoa, Italy.
#'
#' Donor:
#' Stefan Aeberhard, email: stefan@coral.cs.jcu.edu.au
#'
#' @references
#' Dua, D. and Graff, C. (2019).
#' \emph{UCI Machine Learning Repository} \url{http://archive.ics.uci.edu/ml}.
#' Irvine, CA: University of California, School of Information and Computer Science.
NULL

load_task_wine = function(id = "wine") {
  b = as_data_backend(readRDS(system.file("extdata", "wine.rds", package = "mlr3")))
  b$hash = "_mlr3_tasks_wine_"
  TaskClassif$new(id, b, target = "type")
}

#' @include mlr_tasks.R
mlr_tasks$add("wine", load_task_wine)

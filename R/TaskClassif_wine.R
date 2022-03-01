#' @title Wine Classification Task
#'
#' @name mlr_tasks_wine
#' @format [R6::R6Class] inheriting from [TaskClassif].
#' @include mlr_tasks.R
#'
#' @description
#' Wine data set from the UCI machine learning repository (\url{https://archive.ics.uci.edu/ml/datasets/wine}).
#' Results of a chemical analysis of three types of wines grown in the same region in Italy but derived from three different cultivars.
#'
#' @templateVar id wine
#' @template task
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
#' `r format_bib("dua_2017")`
#'
#' @template seealso_task
NULL

load_task_wine = function(id = "wine") {
  b = as_data_backend(readRDS(system.file("extdata", "wine.rds", package = "mlr3")))
  task = TaskClassif$new(id, b, target = "type",
    label = "Wine Regions")
  b$hash = task$man = "mlr3::mlr_tasks_wine"
  task
}

#' @include mlr_tasks.R
mlr_tasks$add("wine", load_task_wine)

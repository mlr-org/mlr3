#' @title Synthetic Diabetes Classification Task
#'
#' @name mlr_tasks_diabetes
#' @format [R6::R6Class] inheriting from [TaskClassif].
#' @include mlr_tasks.R
#'
#' @description
#' A binary classification task for the [mlbench::SynthDiabetes2] data set.
#' Positive class is set to `"pos"`.
#' Several feature columns contain missing values, which makes the task useful for preprocessing examples and tests.
#'
#' The data is fully synthetic and contains no real patient data.
#' It replaces the former `pima` task, which was based on the `PimaIndiansDiabetes2` data set.
#' That data set was removed from \CRANpkg{mlbench} at the request of the institute that conducted the underlying study.
#'
#' @templateVar id diabetes
#' @template task
#'
#' @template seealso_task
NULL

load_task_diabetes = function(id = "diabetes") {
  b = as_data_backend(load_dataset("SynthDiabetes2", "mlbench"))
  task = TaskClassif$new(id, b, target = "diabetes", positive = "pos", label = "Synthetic Diabetes")
  b$hash = task$man = "mlr3::mlr_tasks_diabetes"
  task
}

#' @include mlr_tasks.R
mlr_tasks$add("diabetes", load_task_diabetes)

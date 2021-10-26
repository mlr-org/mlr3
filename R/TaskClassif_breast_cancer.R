#' @title Wisconsin Breast Cancer Classification Task
#'
#' @name mlr_tasks_breast_cancer
#' @format [R6::R6Class] inheriting from [TaskClassif].
#' @include mlr_tasks.R
#'
#' @description
#' A classification task for the [mlbench::BreastCancer] data set.
#'
#' * Column `"Id"` has been removed.
#' * Column names have been converted to `snake_case`.
#' * Positive class is set to `"malignant"`.
#' * 16 incomplete cases have been removed from the data set.
#' * All factor features have been converted to ordered factors.
#'
#' @templateVar id breast_cancer
#' @template task
#'
#' @template seealso_task
NULL

load_task_breast_cancer = function(id = "breast_cancer") {
  tab = load_dataset("BreastCancer", "mlbench")
  names(tab) = tolower(chartr(".", "_", colnames(tab)))
  for (cn in c("bare_nuclei", "bl_cromatin", "normal_nucleoli", "mitoses")) {
    tab[[cn]] = ordered(tab[[cn]])
  }
  b = as_data_backend(remove_named(tab[stats::complete.cases(tab), ], "id"))

  task = TaskClassif$new(id, b, target = "class", positive = "malignant")
  b$hash = task$man = "mlr3::mlr_tasks_breast_cancer"
  task
}

#' @include mlr_tasks.R
mlr_tasks$add("breast_cancer", load_task_breast_cancer)

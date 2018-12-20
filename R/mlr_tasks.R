#' @title Dictionary of Tasks
#'
#' @description
#' A simple [Dictionary] storing objects of class [Task].
#' Each task has an associated help page, see `mlr_tasks_[id]`.
#'
#' @section Usage:
#' See [Dictionary].
#'
#' @name mlr_tasks
#' @family Dictionary
#' @family Task
#' @examples
#' mlr_tasks$ids()
#' as.data.table(mlr_tasks)
#' mlr_tasks$get("iris")
#' head(mlr_tasks$get("iris")$data())
#'
#' # Add a new task, based on a subset of iris:
#' data = iris
#' data$Species = factor(ifelse(data$Species == "setosa", "1", "0"))
#' b = as_data_backend(data)
#' task = TaskClassif$new("iris.binary", b, target = "Species", positive = "1")
#'
#' # add to dictionary
#' mlr_tasks$add("iris.binary", task)
#'
#' # list available tasks
#' mlr_tasks$ids()
#'
#' # retrieve from dictionary
#' mlr_tasks$get("iris.binary")
#'
#' # remove task again
#' mlr_tasks$remove("iris.binary")
NULL

#' @include Dictionary.R
DictionaryTask = R6Class("DictionaryTask",
  inherit = Dictionary,
  cloneable = FALSE
)


#' @export
mlr_tasks = DictionaryTask$new()

#' @export
as.data.table.DictionaryTask = function(x, ...) {
  setkeyv(map_dtr(x$ids(), function(id) {
    t = x$get(id)
    data.table(id = id, type = t$task_type, nrow = t$nrow, ncol = t$ncol)
  }), "id")[]
}

load_dataset = function(id, package, keep.rownames = FALSE) {
  if (!length(find.package(package, quiet = TRUE)))
    stopf("Please install package '%s' for data set '%s'", package, id)
  ee = new.env(parent = emptyenv())
  data(list = id, package = package, envir = ee)
  if (!keep.rownames)
    rownames(ee[[id]]) = NULL
  ee[[id]]
}


#' @title Iris Classification Task
#' @name mlr_tasks_iris
#' @description
#' A classification task for the popular [datasets::iris] data set.
mlr_tasks$add("iris", function() {
  b = as_data_backend(load_dataset("iris", "datasets"))
  TaskClassif$new("iris", b, target = "Species")
})

#' @title Sonar Classification Task
#' @name mlr_tasks_sonar
#' @description
#' A classification task for the [mlbench::Sonar] data set.
#' Positive class is set to "M" (Mine).
mlr_tasks$add("sonar",  function() {
  b = as_data_backend(load_dataset("Sonar", "mlbench"))
  TaskClassif$new("sonar", b, target = "Class", positive = "M")
})

#' @title Boston Housing Regression Task
#' @name mlr_tasks_bh
#' @description
#' A regression task for the [mlbench::BostonHousing2] data set.
mlr_tasks$add("bh",  function() {
  b = as_data_backend(load_dataset("BostonHousing2", "mlbench"))
  TaskRegr$new("boston_housing", b, target = "medv")
})

#' @title Pima Indian Diabetes Classification Task
#' @name mlr_tasks_pima
#' @description
#' A classification task for the [mlbench::PimaIndiansDiabetes2] data set.
#' Positive class is set to "pos".
mlr_tasks$add("pima", function() {
  b = as_data_backend(load_dataset("PimaIndiansDiabetes2", "mlbench"))
  TaskClassif$new("pima_indians", b, target = "diabetes", positive = "pos")
})

#' @title Zoo Classification Task
#' @name mlr_tasks_zoo
#' @description
#' A classification task for the [mlbench::Zoo] data set.
mlr_tasks$add("zoo", function() {
  b = as_data_backend(load_dataset("Zoo", "mlbench", keep.rownames = TRUE))
  TaskClassif$new("zoo", b, target = "type")
})

#' @title Spam Classification Task
#' @name mlr_tasks_spam
#' @description
#' A classification task for the [kernlab::spam] data set.
#' Positive class is set to "spam".
mlr_tasks$add("spam", function() {
  b = as_data_backend(load_dataset("spam", "kernlab"))
  TaskClassif$new("spam", b, target = "type", positive = "spam")
})

#' @title Titanic Classification Task
#' @name mlr_tasks_titanic
#' @description
#' A classification task for the [titanic::titanic] data set.
#'
#' The following preprocessing steps are already performed:
#' * Training and test data has been merged. In the test data, the target variable "Survived" is NA.
#' * Columns `Survived` and `Sex` have been converted to a factor.
#' * Columns `Embarked` and `Cabin` have been converted to a factor and empty string have been replaced with NA.
#' * The positive class is set to "1" (Survived).
mlr_tasks$add("titanic", function() {
  data = rbindlist(list(load_dataset("titanic_train", package = "titanic"), load_dataset("titanic_test", package = "titanic")), fill = TRUE)
  data$Survived = factor(data$Survived, levels = c("0", "1"))
  data$Sex = factor(data$Sex)
  data$Embarked = factor(replace(data$Embarked, !nzchar(data$Embarked), NA))
  data$Cabin = replace(data$Cabin, !nzchar(data$Cabin), NA)
  b = as_data_backend(data)
  TaskClassif$new("titanic", b, target = "Survived", positive = "1")
})

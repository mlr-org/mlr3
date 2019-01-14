#' @title Dictionary of Tasks
#'
#' @name mlr_tasks
#' @description
#' A simple [Dictionary] storing objects of class [Task].
#' Each task has an associated help page, see `mlr_tasks_[id]`.
#'
#' @section Usage:
#' See [Dictionary].
#'
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
    feats = factor(t$feature_types$type, levels = mlr_reflections$task_feature_types)
    feats = as.list(table(feats))
    insert_named(data.table(id = id, type = t$task_type, measures = list(ids(t$measures)),
        nrow = t$nrow, ncol = t$ncol), feats)
  }), "id")[]
}

#' @title Iris Classification Task
#' @name mlr_tasks_iris
#' @description
#' A classification task for the popular [datasets::iris] data set.
mlr_tasks$add("iris", function() {
  b = as_data_backend(load_dataset("iris", "datasets"))
  b$hash = "_mlr_tasks_iris_"
  TaskClassif$new("iris", b, target = "Species")
})

#' @title Sonar Classification Task
#' @name mlr_tasks_sonar
#' @description
#' A classification task for the [mlbench::Sonar] data set.
#' Positive class is set to "M" (Mine).
mlr_tasks$add("sonar",  function() {
  b = as_data_backend(load_dataset("Sonar", "mlbench"))
  b$hash = "_mlr_tasks_sonar_"
  TaskClassif$new("sonar", b, target = "Class", positive = "M")
})

#' @title Boston Housing Regression Task
#' @name mlr_tasks_bh
#' @description
#' A regression task for the [mlbench::BostonHousing2] data set.
mlr_tasks$add("bh",  function() {
  b = as_data_backend(load_dataset("BostonHousing2", "mlbench"))
  b$hash = "_mlr_tasks_bh_"
  TaskRegr$new("boston_housing", b, target = "medv")
})

#' @title "Motor Trend" Car Road Tests Task
#' @name mlr_tasks_mtcars
#' @description
#' A regression task for the [datasets::mtcars] data set.
#' Target variable is `mpg` (Miles/(US) gallon).
mlr_tasks$add("mtcars",  function() {
  b = as_data_backend(load_dataset("mtcars", "datasets"))
  b$hash = "_mlr_tasks_mtcars_"
  TaskRegr$new("mtcars", b, target = "mpg")
})

#' @title Pima Indian Diabetes Classification Task
#' @name mlr_tasks_pima
#' @description
#' A classification task for the [mlbench::PimaIndiansDiabetes2] data set.
#' Positive class is set to "pos".
mlr_tasks$add("pima", function() {
  b = as_data_backend(load_dataset("PimaIndiansDiabetes2", "mlbench"))
  b$hash = "_mlr_tasks_pima_"
  TaskClassif$new("pima_indians", b, target = "diabetes", positive = "pos")
})

#' @title Zoo Classification Task
#' @name mlr_tasks_zoo
#' @description
#' A classification task for the [mlbench::Zoo] data set.
mlr_tasks$add("zoo", function() {
  b = as_data_backend(load_dataset("Zoo", "mlbench", keep_rownames = TRUE))
  b$hash = "_mlr_tasks_zoo_"
  TaskClassif$new("zoo", b, target = "type")
})

#' @title Spam Classification Task
#' @name mlr_tasks_spam
#' @description
#' A classification task for the [kernlab::spam] data set.
#' Positive class is set to "spam".
mlr_tasks$add("spam", function() {
  b = as_data_backend(load_dataset("spam", "kernlab"))
  b$hash = "_mlr_tasks_kernlab_"
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
  modify_if(data, is.character, factor)
  b = as_data_backend(data)
  b$hash = "_mlr_tasks_titanic_"
  task = TaskClassif$new("titanic", b, target = "Survived", positive = "1")
  task$set_row_role(which(is.na(task$truth())), "validation")
})

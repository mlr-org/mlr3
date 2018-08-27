#' @title Dictionary of defined Tasks
#' @docType class
#' @format \code{\link{R6Class}} object
#' @include Dictionary.R
#'
#' @description
#' \code{Tasks} is a \code{\link{Dictionary}} used to manage tasks.
#'
#' @export
#' @examples
#' # List task ids:
#' mlr_tasks$ids
#'
#' # Get a briew summary:
#' as.data.frame(mlr_tasks)
#'
#' # Retrieve a specific task:
#' mlr_tasks$get("iris")
#'
#' # Add a new task, based on a subset of iris:
#' data = iris
#' data$Species = ifelse(data$Species == "setosa", "1", "0")
#' task = TaskClassif$new("iris.binary", data = data, target = "Species")
#' task$classes_n
#' mlr_tasks$add(task)
#' mlr_tasks$remove("iris.binary")
mlr_tasks = Dictionary$new("Task")


mlr_tasks$add(LazyValue("iris", function() {
  TaskClassif$new("iris", data = load_dataset("iris", "datasets"), target = "Species")
}))


mlr_tasks$add(LazyValue("sonar", function() {
  TaskClassif$new("Sonar", data = load_dataset("Sonar", "mlbench"), target = "Class")
}))


mlr_tasks$add(LazyValue("bh", function() {
  TaskRegr$new("BostonHousing2", data = load_dataset("BostonHousing2", "mlbench"), target = "medv")
}))

mlr_tasks$add(LazyValue("pima", function() {
  TaskClassif$new("PimaIndiansDiabetes2", data = load_dataset("PimaIndiansDiabetes2", "mlbench"),
    target = "diabetes", positive = "pos")
}))

mlr_tasks$add(LazyValue("zoo", function() {
  TaskClassif$new("Zoo", data = load_dataset("Zoo", "mlbench", TRUE), target = "type")
}))

mlr_tasks$add(LazyValue("spam", function() {
  TaskClassif$new("spam", data = load_dataset("spam", "kernlab", TRUE), target = "type", positive = "spam")
}))

load_dataset = function(id, package, keep.rownames = FALSE) {
  if (!nzchar(find.package(package, quiet = TRUE)))
    stopf("Please install package '%s' for data set '%s'", package, id)
  ee = new.env(parent = emptyenv())
  data(list = id, package = package, envir = ee)
  if (!keep.rownames)
    rownames(ee[[id]]) = NULL
  ee[[id]]
}

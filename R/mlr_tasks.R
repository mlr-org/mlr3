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
#' # Retrieve a specific task:
#' mlr_tasks$get("iris")
#'
#' # Add a new task, based on a subset of iris:
#' data = iris
#' data$Species = ifelse(data$Species == "setosa", "1", "0")
#' task = TaskClassif$new("iris.binary", data = data, target = "Species")
#' task$class_n
#' mlr_tasks$add(task)
#' mlr_tasks$remove("iris.binary")
mlr_tasks = Dictionary$new("Task")


mlr_tasks$add(LazyValue("iris", function() {
  b = BackendDataTable$new(data = load_dataset("iris", "datasets"))
  TaskClassif$new("iris", b, target = "Species")
}))


mlr_tasks$add(LazyValue("sonar", function() {
  b = BackendDataTable$new(data = load_dataset("Sonar", "mlbench"))
  TaskClassif$new("Sonar", b, target = "Class")
}))


mlr_tasks$add(LazyValue("bh", function() {
  b = BackendDataTable$new(data = load_dataset("BostonHousing2", "mlbench"))
  TaskRegr$new("BostonHousing2", b, target = "medv")
}))

mlr_tasks$add(LazyValue("pima", function() {
  b = BackendDataTable$new(data = load_dataset("PimaIndiansDiabetes2", "mlbench"))
  TaskClassif$new("PimaIndiansDiabetes2",b , target = "diabetes", positive = "pos")
}))

mlr_tasks$add(LazyValue("zoo", function() {
  b = BackendDataTable$new(data = load_dataset("Zoo", "mlbench", TRUE))
  TaskClassif$new("Zoo", b, target = "type")
}))

mlr_tasks$add(LazyValue("spam", function() {
  b = BackendDataTable$new(data = load_dataset("spam", "kernlab"))
  TaskClassif$new("spam", b, target = "type", positive = "spam")
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

#' @title Dictionary of example learning tasks
#'
#' @section Usage:
#' See [Dictionary].
#'
#' @name mlr_tasks
#' @family Dictionary
#' @family Task
#' @examples
#' mlr_tasks$ids()
#' mlr_tasks$get("iris")
#' head(mlr_tasks$get("iris")$data())
#'
#' # Add a new task, based on a subset of iris:
#' data = iris
#' data$Species = ifelse(data$Species == "setosa", "1", "0")
#' b = BackendDataTable$new(data)
#' task = TaskClassif$new("iris.binary", b, target = "Species")
#' mlr_tasks$add(task)
#' mlr_tasks$ids()
#' mlr_tasks$get("iris.binary")
#' mlr_tasks$remove("iris.binary")
NULL

#' @include Dictionary.R
#' @export
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

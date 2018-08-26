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
#' mlr.tasks$ids
#'
#' # Get a briew summary:
#' as.data.frame(mlr.tasks)
#'
#' # Retrieve a specific task:
#' mlr.tasks$get("iris")
#'
#' # Add a new task, based on a subset of iris:
#' data = iris
#' data$Species = ifelse(data$Species == "setosa", "1", "0")
#' task = TaskClassif$new("iris.binary", data = data, target = "Species")
#' task$nclasses
#' mlr.tasks$add(task)
#' mlr.tasks$remove("iris.binary")
mlr.tasks = Dictionary$new("Task")
class(mlr.tasks) = c("DictionaryTasks", class(mlr.tasks))


#' @export
as.data.table.DictionaryTasks = function(x, ...) {
  rbindlist(eapply(x$items, function(obj) {
      list(task.type = obj$task.type, nrow = obj$nrow, ncol = obj$ncol)
  }))
}


#' @export
as.data.frame.DictionaryTasks = function(x, ...) {
  setDF(as.data.table(x))[]
}

mlr.tasks$add(lazyValue("iris", function() {
  TaskClassif$new("iris", data = loadData("iris", "datasets"), target = "Species")
}))


mlr.tasks$add(lazyValue("sonar", function() {
  TaskClassif$new("Sonar", data = loadData("Sonar", "mlbench"), target = "Class")
}))


mlr.tasks$add(lazyValue("bh", function() {
  TaskRegr$new("BostonHousing2", data = loadData("BostonHousing2", "mlbench"), target = "medv")
}))

mlr.tasks$add(lazyValue("pima", function() {
  TaskClassif$new("PimaIndiansDiabetes2", data = loadData("PimaIndiansDiabetes2", "mlbench"),
    target = "diabetes", positive = "pos")
}))

mlr.tasks$add(lazyValue("zoo", function() {
  TaskClassif$new("Zoo", data = loadData("Zoo", "mlbench", TRUE), target = "type")
}))

mlr.tasks$add(lazyValue("spam", function() {
  TaskClassif$new("spam", data = loadData("spam", "kernlab", TRUE), target = "type", positive = "spam")
}))

loadData = function(id, package, keep.rownames = FALSE) {
  if (!nzchar(find.package(package, quiet = TRUE)))
    stopf("Please install package '%s' for data set '%s'", package, id)
  ee = new.env(parent = emptyenv())
  data(list = id, package = package, envir = ee)
  if (!keep.rownames)
    rownames(ee[[id]]) = NULL
  ee[[id]]
}

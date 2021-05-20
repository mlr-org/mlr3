#' @title Palmer Penguins Data Set
#'
#' @name mlr_tasks_penguins
#' @format [R6::R6Class] inheriting from [TaskClassif].
#' @include mlr_tasks.R
#'
#' @description
#' Classification data to predict the species of penguins from the \CRANpkg{palmerpenguins} package, see [palmerpenguins::penguins].
#' A better alternative to the [iris data set][iris].
#'
#' @section Construction:
#' ```
#' mlr_tasks$get("penguins")
#' tsk("penguins")
#' ```
#'
#' @section Meta Information:
#' `r rd_info(tsk("penguins"))`
#'
#' @section Pre-processing:
#' * The unit of measurement have been removed from the column names.
#'   Lengths are given in millimeters (mm), weight in gram (g).
#'
#' @source \CRANpkg{palmerpenguins}
#'
#' @references
#' `r format_bib("gorman2014")`
#'
#' \url{https://github.com/allisonhorst/palmerpenguins}
#'
#' @template seealso_task
NULL

load_task_penguins = function() {
  penguins = as.data.table(palmerpenguins::penguins)
  setnames(penguins,
    old = c("bill_length_mm", "bill_depth_mm", "flipper_length_mm", "body_mass_g"),
    new = c("bill_length", "bill_depth", "flipper_length", "body_mass")
  )

  b = as_data_backend(penguins)
  task = TaskClassif$new("penguins", b, target = "species")
  b$hash = task$man = "mlr3::mlr_tasks_penguins"
  task
}

#' @include mlr_tasks.R
mlr_tasks$add("penguins", load_task_penguins)

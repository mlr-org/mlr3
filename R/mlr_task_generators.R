#' @title Dictionary of Task Generators
#'
#' @name mlr_task_generators
#' @description
#' A simple [Dictionary] storing generator functions returning a [Task].
#'
#' @section Usage:
#' See [Dictionary].
#'
#' @family Dictionary
#' @family Task
#' @family Generator
#' @examples
#' g = mlr_task_generators$get("smiley")
#' task = g$generate(10)
#' print(task)
#' task$data()
NULL

#' @include Dictionary.R
DictionaryTaskGenerators = R6Class("DictionaryTaskGenerators",
  inherit = Dictionary,
  cloneable = FALSE
)

#' @export
mlr_task_generators = DictionaryTaskGenerators$new()

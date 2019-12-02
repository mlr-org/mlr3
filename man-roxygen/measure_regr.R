#' @usage NULL
#' @name mlr_measures_regr.<%= id %>
#' @include mlr_measures.R
#' @format [R6::R6Class()] inheriting from [Measure].
#'
#' @inherit mlr3measures::<%= id %> title description
#' @inheritSection mlr3measures::<%= id %> Meta Information
#'
#' @section Construction:
#' This measures can be retrieved from the dictionary [mlr_measures]:
#' ```
#' mlr_measures$get("classif.<%= id %>")
#' msr("classif.<%= id %>")
#' ```
#'
#' @note
#' The score function calls [mlr3measures::<%= id %>()] from package \CRANpkg{mlr3measures}.
#'
#' If the measure is undefined for the input, `NaN` is returned.
#' This can be customized by setting the field `na_value`.
#'
#' @family regression measures
#'
#' @seealso
#' [Dictionary][mlr3misc::Dictionary] of [Measures][Measure]: [mlr_measures]
#'
#' `as.data.table(mlr_measures)` for a complete table of all (also dynamically created) [Measure] implementations.

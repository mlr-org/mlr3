#' @name mlr_measures_classif.<%= id %>
#' @include mlr_measures.R
#'
#' @inherit mlr3measures::<%= id %> title description details
#' @inheritSection mlr3measures::<%= id %> Meta Information
#'
#' @section Dictionary:
#' This [Measure] can be instantiated via the [dictionary][mlr3misc::Dictionary] [mlr_measures] or with the associated sugar function [msr()]:
#' ```
#' mlr_measures$get("<%= id %>")
#' msr("<%= id %>")
#' ```
#'
#' @note
#' The score function calls [mlr3measures::<%= id %>()] from package \CRANpkg{mlr3measures}.
#'
#' If the measure is undefined for the input, `NaN` is returned.
#' This can be customized by setting the field `na_value`.
#'
#' @family classification measures
#' @family multiclass classification measures
#'
#' @seealso
#' [Dictionary][mlr3misc::Dictionary] of [Measures][Measure]: [mlr_measures]
#'
#' `as.data.table(mlr_measures)` for a complete table of all (also dynamically created) [Measure] implementations.

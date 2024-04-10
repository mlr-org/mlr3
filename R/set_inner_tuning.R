#' @title Activate Inner Tuning
#' @description
#' This generic helps to (de)activate the inner tuning for a learner.
#'
#' @details
#'
#' Calling this function does not autoconfigure the inner tuning of the learner.
#' However, if the necessary parameter to configure the inner tuning are neither set nor specified via the `...`
#' parameter, the function should raise an informative error message.
#'
#' Disabling the inner tuning needs to happen automatically, as it is required to make the [`mlr3tuning::AutoTuner`]
#' work.
#'
#' Many learners need to ensure that validation is enabled to allow for inner tuning, however there are some learners
#' that don't have the `"validation"` property but can still do inner tuning. Examples include `cv.glmnet`.
#'
#' @param learner ([`Learner`])\cr
#'   The learner for which to set the inner tuning.
#' @param disable (`logical(1)`)\cr
#'   Whether to disable the inner tuning, default is `FALSE`.
#' @param ... (any)\cr
#'   Additional parameter values that are set in the learner.
#' @return The input [`Learner`]
#' @export
set_inner_tuning = function(learner, disable = FALSE, ...) {
  UseMethod("set_inner_tuning")
}

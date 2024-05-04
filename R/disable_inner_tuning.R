#' @title Disable Inner Tuning
#'
#' @description
#' Disable the inner tuning for some parameters of a [`Learner`].
#'
#' @details
#' When implementing inner tuning for a [`Learner`], this method must be implemented.
#' This is necessary to be able to use inner tuning with the [`mlr3tuning::AutoTuner`], which -- after using
#' inner tuning during to find the best hyperparameters -- then needs to deactive it for the final model fit.
#'
#' Note that calling this function should not change the value of the `$validate` parameter.
#'
#' @param learner ([`Learner`])\cr
#'   The learner.
#' @param ids (`character()`)\cr
#'   IDs of the parameters for which to disable inner tuning.
#' @param ... (any)\cr
#'   Additional arguments.
#' @return Modified [`Learner`]
#' @export
#' @examples
#' learner = lrn("classif.debug", validate = 0.3, early_stopping = TRUE, iter = 100)
#' disable_inner_tuning(learner, "iter")
#' learner$param_set$values
#' learner$validate
disable_inner_tuning = function(learner, ids, ...) {
  assert_subset(ids, names(learner$param_set$tags)[map_lgl(learner$param_set$tags, function(x) "inner_tuning" %in% x)])
  UseMethod("disable_inner_tuning")
}

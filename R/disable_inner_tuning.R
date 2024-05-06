#' @title Disable Internal Tuning
#'
#' @description
#' Disable the internal tuning for some parameters of a [`Learner`].
#'
#' @details
#' When implementing internal tuning for a [`Learner`], this method must be implemented.
#' This is necessary to be able to use internal tuning with the [`mlr3tuning::AutoTuner`], which -- after using
#' internal tuning during to find the best hyperparameters -- then needs to deactive it for the final model fit.
#'
#' Note that calling this function should not change the value of the `$validate` parameter.
#'
#' @param learner ([`Learner`])\cr
#'   The learner.
#' @param ids (`character()`)\cr
#'   IDs of the parameters for which to disable internal tuning.
#' @param ... (any)\cr
#'   Additional arguments.
#' @return Modified [`Learner`]
#' @export
#' @keywords internal
#' @examples
#' learner = lrn("classif.debug", validate = 0.3, early_stopping = TRUE, iter = 100)
#' disable_internal_tuning(learner, "iter")
#' learner$param_set$values
#' learner$validate
disable_internal_tuning = function(learner, ids, ...) {
  assert_subset(ids, names(learner$param_set$tags)[map_lgl(learner$param_set$tags, function(x) "internal_tuning" %in% x)])
  UseMethod("disable_internal_tuning")
}

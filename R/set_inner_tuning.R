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
#' @param ids (`character(1)`)\cr
#'   The ids of the parameters to disable. Will be ignored by [`Learner`]s that have only one such parameter.
#'   Mostly relevant for [`mlr3pipelines::GraphLearner`].
#' @param param_vals (named `list()`)\cr
#'   Parameter values for the learner.
#' @param ... (any)\cr
#'   Additional arguments.
#' @return The input [`Learner`]
#' @export
set_inner_tuning = function(learner, disable = FALSE, ids = NULL, param_vals = named_list(), ...) {
  assert_flag(disable)
  if (!is.null(ids)) {
    assert_subset(ids, names(learner$param_set$tags[map_lgl(learner$param_set$tags, function(t) "inner_tuning" %in% t)]))
  }
  UseMethod("set_inner_tuning")
}

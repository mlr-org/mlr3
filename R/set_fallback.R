#' @title Set a Fallback Learner
#'
#' @description
#' Set a fallback learner for a given learner.
#' The function searches for a suitable fallback learner based on the task type.
#' Additional checks are performed to ensure that the fallback learner supports the predict type.
#'
#' @param learner [Learner]\cr
#'  The learner for which a fallback learner should be set.
#'
#' @return
#' Returns the learner itself, but modified **by reference**.
set_fallback = function(learner) {
  assert_learner(learner)

  # search for suitable fallback learner
  fallback_id = mlr_reflections$learner_fallback[[learner$task_type]]

  if (is.null(fallback_id)) {
    stopf("No fallback learner available for task type '%s'.", learner$task_type)
  }

  fallback = lrn(fallback_id)

  # set predict type
  if (learner$predict_type %nin% fallback$predict_types) {
    stopf("Fallback learner '%s' does not support predict type '%s'.", fallback_id, learner$predict_type)
  }

  fallback$predict_type = learner$predict_type

  # set quantiles
  if (learner$predict_type == "quantiles") {

    if (is.null(learner$quantiles) || is.null(learner$quantile_response)) {
      stopf("Cannot set quantiles for fallback learner. Set `$quantiles` and `$quantile_response` in %s.", learner$id)
    }

    fallback$quantiles = learner$quantiles
    fallback$quantile_response = learner$quantile_response
  }

  learner$fallback = fallback
  return(learner)
}

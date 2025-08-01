#' @title Create a Fallback Learner
#'
#' @description
#' Create a fallback learner for a given learner.
#' The function searches for a suitable fallback learner based on the task type.
#' Additional checks are performed to ensure that the fallback learner supports the predict type.
#'
#' @param learner [Learner]\cr
#'  The learner for which a fallback learner should be created.
#' @param ... `any`\cr
#' ignored.
#'
#' @return [Learner]
#' @export
default_fallback = function(learner, ...) {
  UseMethod("default_fallback")
}

#' @rdname default_fallback
#' @export
default_fallback.Learner = function(learner, ...) {
  # FIXME: remove when new encapsulate/fallback system is in place
  return(NULL)
}

#' @rdname default_fallback
#' @export
default_fallback.LearnerClassif = function(learner, ...) {
  fallback = lrn("classif.featureless")

  # set predict type
  if (learner$predict_type %nin% fallback$predict_types) {
    stopf("Fallback learner '%s' does not support predict type '%s'.", fallback$id, learner$predict_type)
  }

  fallback$predict_type = learner$predict_type

  return(fallback)
}

#' @rdname default_fallback
#' @export
default_fallback.LearnerRegr = function(learner, ...) {
  fallback = lrn("regr.featureless")

  # set predict type
  if (learner$predict_type %nin% fallback$predict_types) {
    stopf("Fallback learner '%s' does not support predict type '%s'.", fallback$id, learner$predict_type)
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

  return(fallback)
}

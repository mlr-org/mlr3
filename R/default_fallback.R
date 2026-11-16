#' @title Create a Fallback Learner
#'
#' @description
#' Create a fallback learner for a given learner.
#' The function searches for a suitable fallback learner based on the task type.
#' Additional checks are performed to ensure that the fallback learner supports the predict type.
#'
#' Learners that inherit directly from [Learner], e.g., `GraphLearner` of \CRANpkg{mlr3pipelines}
#' or `AutoTuner` of \CRANpkg{mlr3tuning}, use the method of the learner class that is registered for their task type
#' in [`mlr_reflections$task_types`][mlr_reflections].
#' An error is raised if no fallback learner is available for the task type of the learner.
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
  task_type = learner$task_type
  learner_class = mlr_reflections$task_types[list(task_type), "learner", on = "type", with = FALSE][[1L]][1L]
  # methods of inheriting classes were already tried by dispatch, avoid infinite call loop
  # also handles task types associated with a plain Learner (e.g. unsupervised)
  method = if (!inherits(learner, learner_class)) getS3method("default_fallback", learner_class, optional = TRUE)
  if (is.null(method)) {
    error_config("Could not find default fallback learner for learner '%s' with task type '%s'.", learner$id, task_type)
  }
  method(learner, ...)
}

#' @rdname default_fallback
#' @export
default_fallback.LearnerClassif = function(learner, ...) {
  fallback = lrn("classif.featureless")

  # set predict type
  if (learner$predict_type %nin% fallback$predict_types) {
    error_config("Fallback learner '%s' does not support predict type '%s'.", fallback$id, learner$predict_type)
  }

  fallback$predict_type = learner$predict_type

  fallback
}

#' @rdname default_fallback
#' @export
default_fallback.LearnerRegr = function(learner, ...) {
  fallback = lrn("regr.featureless")

  # set predict type
  if (learner$predict_type %nin% fallback$predict_types) {
    error_config("Fallback learner '%s' does not support predict type '%s'.", fallback$id, learner$predict_type)
  }

  fallback$predict_type = learner$predict_type

  # set quantiles
  if (learner$predict_type == "quantiles") {
    if (is.null(learner$quantiles) || is.null(learner$quantile_response)) {
      error_config(
        "Cannot set quantiles for fallback learner. Set `$quantiles` and `$quantile_response` in %s.",
        learner$id
      )
    }

    fallback$quantiles = learner$quantiles
    fallback$quantile_response = learner$quantile_response
  }

  fallback
}

#' @title Fast Aggregation of ResampleResults and BenchmarkResults
#'
#' @description
#' Aggregates a [BenchmarkResult] for a single simple measure.
#' Returns the aggregated score for each uhash.
#'
#' @details
#' This function is faster than `$aggregate()` because it does not reassemble the resampling results.
#' It also does not require the task, learner, model or train set to be available.
#' It only works for simple measures.
#'
#' @param obj ([ResampleResult] | [BenchmarkResult]).
#' @param measure ([Measure]).
#' @export
faggregate = function(obj, measure) {
  if (any(c("requires_task", "requires_learner", "requires_model", "requires_train_set") %in% measure$properties)) {
    stopf("Cannot aggregate measure that requires task, learner, model or train set")
  }

  tab = fscore(obj, measure)
  aggregator = measure$aggregator %??% mean
  set_names(tab[, list(score = aggregator(get(measure$id))), by = "uhash"], c("uhash", measure$id))
}

#' @title Fast Scoring of ResampleResults and BenchmarkResults
#'
#' @description
#' Scores a [ResampleResult] or [BenchmarkResult] for a single simple measure.
#' Returns the score for each iteration and uhash.
#'
#' @details
#' This function is faster than `$score()` because it does not reassemble the resampling results.
#' It also does not require the task, learner, model or train set to be available.
#' It only works for simple measures.
#'
#' @param obj ([ResampleResult] | [BenchmarkResult]).
#' @param measure ([Measure]).
#' @export
fscore = function(obj, measure) {
  tab = get_private(obj)$.data$data$fact[, c("iteration", "prediction", "uhash"), with = FALSE]
  set(tab, j = measure$id, value = map_dbl(tab$prediction, fscore_single_measure, measure = measure))
  tab[, c("iteration", "uhash", measure$id), with = FALSE]
}

fscore_single_measure = function(measure, prediction) {
  if (!length(measure$predict_sets)) {
    score = get_private(measure)$.score(prediction = NULL, task = NULL)
    return(score)
  }

  # merge multiple predictions (on different predict sets) to a single one
  if (is.list(prediction)) {
    ii = match(measure$predict_sets, names(prediction))
    if (anyMissing(ii)) {
      return(NaN)
    }
    prediction = do.call(c, prediction[ii])
  }

  # convert pdata to regular prediction
  prediction = as_prediction(prediction, check = FALSE)

  if (is.null(prediction) && length(measure$predict_sets)) {
    return(NaN)
  }

  if (!is_scalar_na(measure$predict_type) && measure$predict_type %nin% prediction$predict_types) {
    return(NaN)
  }

  get_private(measure)$.score(prediction = prediction, task = NULL, weights = if (measure$use_weights == "use") prediction$weights)
}

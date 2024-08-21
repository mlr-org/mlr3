#' @title Convert to ResampleResult
#'
#' @description
#' Convert object to a [ResampleResult].
#'
#' The S3 method for `list` expects argument `x` to be a list of [Prediction] objects and
#' all other relevant objects ([Task], [Learner]s, and instantiated [Resampling]) must
#' be provided, too.
#' A more flexible way to manually create a [ResampleResult] is implemented in [as_result_data()].
#'
#' @param x (any)\cr
#'  Object to convert.
#' @param ... (any)\cr
#'  Currently not used.
#'
#' @return ([ResampleResult]).
#' @export
as_resample_result = function(x, ...) {
  UseMethod("as_resample_result")
}


#' @rdname as_resample_result
#' @export
as_resample_result.ResampleResult = function(x, ...) { # nolint
  x
}

#' @rdname as_resample_result
#' @param view (`character()`)\cr
#'   See construction argument `view` of [`ResampleResult`].
#' @export
as_resample_result.ResultData = function(x, view = NULL, ...) { # nolint
  ResampleResult$new(x, view = view)
}

#' @rdname as_resample_result
#' @inheritParams as_result_data
#' @export
as_resample_result.list = function(x, task, learners, resampling, store_backends = TRUE, ...) { # nolint
  rdata = as_result_data(task = task, learners = learners, resampling = resampling,
    iterations = seq_len(resampling$iters), predictions = x)

  ResampleResult$new(rdata)
}

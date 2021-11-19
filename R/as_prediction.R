#' @title Convert to a Prediction
#'
#' @description
#' Convert object to a [Prediction] or a list of [Prediction].
#'
#' @inheritParams as_prediction_data
#'
#' @return [Prediction].
#' @export
as_prediction = function(x, check = TRUE, ...) {
  UseMethod("as_prediction")
}

#' @rdname as_prediction
#' @export
as_prediction.Prediction = function(x, check = TRUE, ...) { # nolint
  x
}


#' @rdname as_prediction
#' @export
as_prediction.PredictionDataClassif = function(x, check = TRUE, ...) { # nolint
  invoke(PredictionClassif$new, check = check, .args = x)
}


#' @rdname as_prediction
#' @export
as_prediction.PredictionDataRegr = function(x, check = TRUE, ...) { # nolint
  invoke(PredictionRegr$new, check = check, .args = x)
}


#' @rdname as_prediction
#' @template param_predict_sets
#' @export
as_predictions = function(x, predict_sets = "test", ...) {
  UseMethod("as_predictions")
}

#' @rdname as_prediction
#' @export
as_predictions.list = function(x, predict_sets = "test", ...) { # nolint
  assert_subset(predict_sets, mlr_reflections$predict_sets)

  result = vector("list", length(x))
  ii = lengths(x) > 0L
  result[ii] = map(x[ii], function(li) {
    assert_list(li, "PredictionData")
    combined = do.call(c, discard(li[predict_sets], is.null))
    if (is.null(combined)) {
      list()
    } else {
      as_prediction(combined, check = FALSE)
    }
  })
  result
}

#' @title Convert to a Prediction
#'
#' @description
#' Convert object to a [Prediction] or a list of [Prediction].
#'
#' @inheritParams as_prediction_data
#'
#' @return [Prediction].
#' @export
as_prediction = function(x, check = FALSE, ...) {
  if (is.null(x)) return(list())

  UseMethod("as_prediction")
}

#' @rdname as_prediction
#' @export
as_prediction.Prediction = function(x, check = FALSE, ...) { # nolint
  x
}


#' @rdname as_prediction
#' @export
as_prediction.PredictionDataClassif = function(x, check = FALSE, ...) { # nolint
  invoke(PredictionClassif$new, check = check, .args = x)
}


#' @rdname as_prediction
#' @export
as_prediction.PredictionDataRegr = function(x, check = FALSE, ...) { # nolint
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
  result = replicate(length(x), list())
  ii = lengths(x) > 0L
  result[ii] = map(x[ii], function(li) {
    assert_list(li, "PredictionData")
    li = discard(li[predict_sets], is.null)
    if (length(li) == 0L) {
      return(list())
    }

    if (length(li) == 1L) {
      combined = li[[1L]]
    } else {
      combined = do.call(c, li)
    }
    as_prediction(combined, check = FALSE)
  })
  result
}

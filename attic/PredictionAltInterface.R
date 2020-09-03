#' @rdname PredictionRegr
#' @export
PredictionRegrNew = R6Class("PredictionRegr", inherit = PredictionRegr,
  public = list(
    #' @param pdata ([PredictionData])\cr
    #'   Named list with the following elements:
    #'
    #'   * `row_ids` (`integer()`)\cr
    #'     Row ids of the predicted observations, i.e. the row ids of the test set.
    #'   * `truth` (`numeric()`)\cr
    #'     True (observed) response.
    #'   * `response` (`numeric()` | `NULL`)\cr
    #'     Vector of numeric response values.
    #'     One element for each observation in the test set.
    #'   * `se` (`numeric()` | `NULL`)\cr
    #'     Numeric vector of predicted standard errors.
    #'     One element for each observation in the test set.
    #'   * `distr` ([distr6::VectorDistribution] | `NULL`)\cr
    #'     [VectorDistribution][distr6::VectorDistribution] from \CRANpkg{distr6}.
    #'     Each individual distribution in the vector represents the random variable 'survival time'
    #'     for an individual observation.
    #'
    #' @param check (`logical(1)`)\cr
    #'   If `TRUE`, performs some argument checks and predict type conversions.
    initialize = function(pdata, check = TRUE) {
      assert_flag(check)
      if (check) {
        pdata = check_prediction_data(pdata)
      }

      self$data = pdata
      self$predict_types = intersect(c("response", "se", "distr"), names(pdata))
    }
  )
)


#' @rdname PredictionClassif
#' @export
PredictionClassifNew = R6Class("PredictionClassif", inherit = PredictionClassif,
  public = list(
    #' @param pdata ([PredictionData])\cr
    #'   Named list with the following elements:
    #'
    #'   * `row_ids` (`integer()`)\cr
    #'     Row ids of the predicted observations, i.e. the row ids of the test set.
    #'   * `truth` (`factor()`)\cr
    #'     True (observed) labels. See the note on manual construction.
    #'   * `response` (`character()` | `factor()` | `NULL`)\cr
    #'     Vector of predicted class labels.
    #'     One element for each observation in the test set.
    #'     Character vectors are automatically converted to factors.
    #'     See the note on manual construction.
    #'   * `prob` (`matrix()` | `NULL`)\cr
    #'     Numeric matrix of posterior class probabilities with one column for each class
    #'     and one row for each observation in the test set.
    #'     Columns must be named with class labels, row names are automatically removed.
    #'     If `prob` is provided, but `response` is not, the class labels are calculated from
    #'     the probabilities using [max.col()] with `ties.method` set to `"random"`.
    #'
    #' @param check (`logical(1)`)\cr
    #'   If `TRUE`, performs some argument checks and predict type conversions.
    initialize = function(pdata, check = TRUE) {
      assert_flag(check)
      if (check) {
        pdata = check_prediction_data(pdata)
      }

      self$data = pdata
      self$predict_types = intersect(c("response", "prob"), names(pdata))
    }
  )
)


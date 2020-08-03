#' @title Prediction Object for Regression
#'
#' @include Prediction.R
#'
#' @description
#' This object wraps the predictions returned by a learner of class [LearnerRegr], i.e.
#' the predicted response and standard error.
#' Additionally, probability distributions implemented in \CRANpkg{distr6} are supported.
#'
#' @family Prediction
#' @export
#' @examples
#' task = tsk("boston_housing")
#' learner = lrn("regr.featureless", predict_type = "se")
#' p = learner$train(task)$predict(task)
#' p$predict_types
#' head(as.data.table(p))
PredictionRegr = R6Class("PredictionRegr", inherit = Prediction,
  cloneable = FALSE,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param task ([TaskRegr])\cr
    #'   Task, used to extract defaults for `row_ids` and `truth`.
    #'
    #' @param row_ids (`integer()`)\cr
    #'   Row ids of the predicted observations, i.e. the row ids of the test set.
    #'
    #' @param truth (`numeric()`)\cr
    #'   True (observed) response.
    #'
    #' @param response (`numeric()`)\cr
    #'   Vector of numeric response values.
    #'   One element for each observation in the test set.
    #'
    #' @param se (`numeric()`)\cr
    #'   Numeric vector of predicted standard errors.
    #'   One element for each observation in the test set.
    #'
    #' @param distr ([distr6::VectorDistribution])\cr
    #'   [VectorDistribution][distr6::VectorDistribution] from \CRANpkg{distr6}.
    #'   Each individual distribution in the vector represents the random variable 'survival time'
    #'   for an individual observation.
    initialize = function(task = NULL, row_ids = task$row_ids, truth = task$truth(), response = NULL, se = NULL, distr = NULL) {
      row_ids = assert_row_ids(row_ids)
      n = length(row_ids)
      self$data = named_list(c("tab", "distr"))

      assert_numeric(response, len = n, any.missing = FALSE, null.ok = TRUE)
      assert_numeric(se, len = n, lower = 0, any.missing = FALSE, null.ok = TRUE)

      if (!is.null(distr)) {
        self$data$distr = assert_class(distr, "VectorDistribution")

        if (is.null(response)) {
          response = unname(distr$mean())
        }

        if (is.null(se)) {
          se = unname(distr$stdev())
        }
      }

      self$task_type = "regr"
      self$predict_types = c("response", "se", "distr")[
        c(!is.null(response), !is.null(se), !is.null(distr))
      ]
      self$data$tab = data.table(
        row_id = row_ids,
        truth = assert_numeric(truth, len = n, null.ok = TRUE),
        response = response,
        se = se
      )

      self$man = "mlr3::PredictionRegr"
    }
  ),

  active = list(
    #' @field response (`numeric()`)\cr
    #' Access the stored predicted response.
    response = function(rhs) {
      assert_ro_binding(rhs)
      self$data$tab$response %??% rep(NA_real_, nrow(self$data$tab))
    },

    #' @field se (`numeric()`)\cr
    #' Access the stored standard error.
    se = function(rhs) {
      assert_ro_binding(rhs)
      self$data$tab$se %??% rep(NA_real_, nrow(self$data$tab))
    },

    #' @field distr ([distr6::VectorDistribution])\cr
    #' Access the stored vector distribution.
    #' Requires package \CRANpkg{distr6}.
    distr = function() {
      require_namespaces("distr6")
      self$data$distr
    },

    #' @field missing (`integer()`)\cr
    #'   Returns `row_ids` for which the predictions are missing or incomplete.
    missing = function(rhs) {
      assert_ro_binding(rhs)
      miss = logical(nrow(self$data$tab))

      if ("response" %in% self$predict_types) {
        miss = is.na(self$response)
      }

      if ("se" %in% self$predict_types) {
        miss = miss | is.na(self$data$tab$se)
      }

      self$data$tab$row_id[miss]
    }
  )
)

#' @export
as.data.table.PredictionRegr = function(x, ...) {
  tab = copy(x$data$tab)
  if ("distr" %in% x$predict_types) {
    require_namespaces("distr6")
    tab$distr = list(x$distr)
  }
  tab
}

#' @export
c.PredictionRegr = function(..., keep_duplicates = TRUE) {
  dots = list(...)
  assert_list(dots, "PredictionRegr")
  assert_flag(keep_duplicates)
  if (length(dots) == 1L) {
    return(dots[[1L]])
  }

  predict_types = map(dots, "predict_types")
  if (!every(predict_types[-1L], setequal, y = predict_types[[1L]])) {
    stopf("Cannot rbind predictions: Probabilities for some predictions, not all")
  }

  tab = map_dtr(dots, function(p) p$data$tab, .fill = FALSE)

  if (any(map_lgl(predict_types, `%in%`, x = "distr"))) {
    require_namespaces("distr6")
    distr = do.call(c, map(dots, "distr"))
  } else {
    distr = NULL
  }

  if (!keep_duplicates) {
    tab = unique(tab, by = "row_id", fromLast = TRUE)
  }

  PredictionRegr$new(row_ids = tab$row_id, truth = tab$truth, response = tab$response, se = tab$se, distr = distr)
}

#' @title Regression Tree Learner
#'
#' @name mlr_learners_regr.rpart
#' @include LearnerRegr.R
#'
#' @description
#' A [LearnerRegr] for a regression tree implemented in [rpart::rpart()] in package \CRANpkg{rpart}.
#'
#' @section Initial parameter values:
#' * Parameter `xval` is initialized to 0 in order to save some computation time.
#'
#' @section Custom mlr3 parameters:
#' * Parameter `model` has been renamed to `keep_model`.
#'
#' @templateVar id regr.rpart
#' @template learner
#'
#' @references
#' `r format_bib("breiman_1984")`
#'
#' @template seealso_learner
#' @export
LearnerRegrRpart = R6Class("LearnerRegrRpart", inherit = LearnerRegr,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      ps = ps(
        cp             = p_dbl(0, 1, default = 0.01, tags = "train"),
        keep_model     = p_lgl(default = FALSE, tags = "train"),
        maxcompete     = p_int(0L, default = 4L, tags = "train"),
        maxdepth       = p_int(1L, 30L, default = 30L, tags = "train"),
        maxsurrogate   = p_int(0L, default = 5L, tags = "train"),
        minbucket      = p_int(1L, tags = "train"),
        minsplit       = p_int(1L, default = 20L, tags = "train"),
        surrogatestyle = p_int(0L, 1L, default = 0L, tags = "train"),
        usesurrogate   = p_int(0L, 2L, default = 2L, tags = "train"),
        xval           = p_int(0L, default = 10L, tags = "train")
      )
      ps$values = list(xval = 0L)

      super$initialize(
        id = "regr.rpart",
        feature_types = c("logical", "integer", "numeric", "factor", "ordered"),
        predict_types = "response",
        packages = "rpart",
        param_set = ps,
        properties = c("weights", "missings", "importance", "selected_features"),
        label = "Regression Tree",
        man = "mlr3::mlr_learners_regr.rpart"
      )
    },

    #' @description
    #' The importance scores are extracted from the model slot `variable.importance`.
    #' @return Named `numeric()`.
    importance = function() {
      if (is.null(self$model)) {
        stopf("No model stored")
      }
      # importance is only present if there is at least on split
      sort(self$model$variable.importance %??% set_names(numeric()), decreasing = TRUE)
    },

    #' @description
    #' Selected features are extracted from the model slot `frame$var`.
    #' @return `character()`.
    selected_features = function() {
      if (is.null(self$model)) {
        stopf("No model stored")
      }
      setdiff(self$model$frame$var, "<leaf>")
    }
  ),

  private = list(
    .train = function(task) {
      pv = self$param_set$get_values(tags = "train")
      names(pv) = replace(names(pv), names(pv) == "keep_model", "model")
      if ("weights" %in% task$properties) {
        pv = insert_named(pv, list(weights = task$weights$weight))
      }

      invoke(rpart::rpart, formula = task$formula(), data = task$data(), .args = pv, .opts = allow_partial_matching)
    },

    .predict = function(task) {
      pv = self$param_set$get_values(tags = "predict")
      newdata = task$data(cols = task$feature_names)
      response = invoke(predict, self$model, newdata = newdata,
        .opts = allow_partial_matching, .args = pv)
      list(response = unname(response))
    }
  )
)

#' @export
default_values.LearnerRegrRpart = function(x, search_space, task, ...) { # nolint
  special_defaults = list(
    minbucket = round(20 / 3)
  )
  defaults = insert_named(default_values(x$param_set), special_defaults)
  defaults[search_space$ids()]
}

#' @include mlr_learners.R
mlr_learners$add("regr.rpart", function() LearnerRegrRpart$new())

#' @title Classification Tree Learner
#'
#' @name mlr_learners_classif.rpart
#' @include LearnerClassif.R
#'
#' @description
#' A [LearnerClassif] for a classification tree implemented in [rpart::rpart()] in package \CRANpkg{rpart}.
#' Parameter `xval` is set to 0 in order to save some computation time.
#' Parameter `model` has been renamed to `keep_model`.
#'
#' @templateVar id classif.rpart
#' @template section_dictionary_learner
#'
#' @section Meta Information:
#' `r rd_info(lrn("classif.rpart"))`
#'
#' @section Parameters:
#' `r rd_info(lrn("classif.rpart")$param_set)`
#'
#' @references
#' `r format_bib("breiman_1984")`
#'
#' @template seealso_learner
#' @export
LearnerClassifRpart = R6Class("LearnerClassifRpart", inherit = LearnerClassif,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      ps = ps(
        minsplit = p_int(default = 20L, lower = 1L, tags = "train"),
        minbucket = p_int(lower = 1L, tags = "train"),
        cp = p_dbl(default = 0.01, lower = 0, upper = 1, tags = "train"),
        maxcompete = p_int(default = 4L, lower = 0L, tags = "train"),
        maxsurrogate = p_int(default = 5L, lower = 0L, tags = "train"),
        maxdepth = p_int(default = 30L, lower = 1L, upper = 30L, tags = "train"),
        usesurrogate = p_int(default = 2L, lower = 0L, upper = 2L, tags = "train"),
        surrogatestyle = p_int(default = 0L, lower = 0L, upper = 1L, tags = "train"),
        xval = p_int(default = 10L, lower = 0L, tags = "train"),
        keep_model = p_lgl(default = FALSE, tags = "train")
      )
      ps$values = list(xval = 0L)

      super$initialize(
        id = "classif.rpart",
        packages = "rpart",
        feature_types = c("logical", "integer", "numeric", "factor", "ordered"),
        predict_types = c("response", "prob"),
        param_set = ps,
        properties = c("twoclass", "multiclass", "weights", "missings", "importance", "selected_features"),
        man = "mlr3::mlr_learners_classif.rpart"
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
      newdata = task$data(cols = task$feature_names)
      response = prob = NULL

      if ("response" %in% self$predict_type) {
        response = invoke(predict, self$model, newdata = newdata, type = "class", .opts = allow_partial_matching)
        response = unname(response)
      } else if ("prob" %in% self$predict_type) {
        prob = invoke(predict, self$model, newdata = newdata, type = "prob", .opts = allow_partial_matching)
        rownames(prob) = NULL
      }

      list(response = response, prob = prob)
    }
  )
)

#' @include mlr_learners.R
mlr_learners$add("classif.rpart", LearnerClassifRpart)

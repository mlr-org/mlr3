#' @include LearnerClassif.R
LearnerClassifUnittest = R6Class("LearnerClassifUnittest", inherit = LearnerClassif,
  public = list(
    initialize = function(id = "classif.unittest") {
      super$initialize(
        id = id,
        feature_types = c("logical", "integer", "numeric", "character", "factor", "ordered"),
        predict_types = c("response", "prob"),
        param_set = ParamSet$new(
          params = list(
            ParamLgl$new("message_train", tags = "train"),
            ParamLgl$new("message_predict", tags = "predict"),
            ParamLgl$new("warning_train", tags = "train"),
            ParamLgl$new("warning_predict", tags = "predict"),
            ParamLgl$new("error_train", tags = "train"),
            ParamLgl$new("error_predict", tags = "predict"),
            ParamLgl$new("segfault_train", tags = "train"),
            ParamLgl$new("segfault_predict", tags = "predict")
          )
        ),
        properties = "missings"
      )
    },

    train = function(task) {
      pv = self$param_vals
      if (isTRUE(pv$message_train))
        message("Message from classif.unittest->train()")
      if (isTRUE(pv$warning_train))
        warning("Warning from classif.unittest->train()")
      if (isTRUE(pv$error_train))
        stop("Error from classif.unittest->train()")
      if (isTRUE(pv$segfault_train))
        get("attach")( structure(list(), class = "UserDefinedDatabase")  )

      label = sample(task$truth(), 1L)
      structure(as.character(label), class = "unittest.model")
    },

    predict = function(model, task, ...) {
      pv = self$param_vals
      if (isTRUE(pv$message_predict))
        message("Message from classif.unittest->predict()")
      if (isTRUE(pv$warning_predict))
        warning("Warning from classif.unittest->predict()")
      if (isTRUE(pv$error_predict))
        stop("Error from classif.unittest->predict()")
      if (isTRUE(pv$segfault_predict))
        get("attach")( structure(list(), class = "UserDefinedDatabase")  )

      PredictionClassif$new(response = rep.int(unclass(model), task$nrow))
    }
  )
)

#' @include mlr_learners.R
mlr_learners$add("classif.unittest", LearnerClassifUnittest)

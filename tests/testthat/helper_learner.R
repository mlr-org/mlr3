if (!mlr_learners$has("classif.verbose")) {
  LearnerClassifVerbose = R6::R6Class("LearnerClassifVerbose", inherit = LearnerClassifDummy,
    public = list(
      initialize = function(id = "classif.verbose") {
        super$initialize(id = id)
        self$par_set = paradox::ParamSet$new(
            params = list(
              paradox::ParamFlag$new("message"),
              paradox::ParamFlag$new("warning")
            )
          )
      },

      train = function(task, ...) {
        if (isTRUE(self$par_vals$message))
          message("Message from classif.verbose$train()")
        if (isTRUE(self$par_vals$warning))
          warning("Warning from classif.verbose$train()")
        super$train(task, ...)
      },

      predict = function(model, task, ...) {
        if (isTRUE(self$par_vals$message))
          message("Message from classif.verbose$predict()")
        if (isTRUE(self$par_vals$warning))
          warning("Warning from classif.verbose$predict()")
        super$predict(model, task, ...)
      }
    )
  )

  mlr_learners$add("classif.verbose", LearnerClassifVerbose)
}

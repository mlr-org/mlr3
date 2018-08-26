#' @title Class for Learners
#' @format \code{\link{R6Class}} object
#'
#' @description
#' A \code{\link[R6]{R6Class}} to construct learners.
#'
#' @return [\code{\link{Learner}}].
#' @include capabilities.R
#' @family Learner
#' @export
Learner = R6Class("Learner",
  public = list(
    id = NULL,
    name = NULL,
    task.type = NULL,
    packages = NULL,
    par.set = NULL,
    properties = NULL,
    train = NULL,
    predict = NULL,
    model.extractors = list(),

    initialize = function(task.type, name, par.set, par.vals, packages, properties, train, predict, model.extractors, predict.type) {
      self$task.type = assertString(task.type)
      self$name = assertString(name)
      self$id = stri_paste(task.type, ".", name)
      self$par.set = assertClass(par.set, "ParamSet")
      private$pv = assertList(par.vals, names = "unique")
      self$packages = assertCharacter(packages, any.missing = FALSE, unique = TRUE)
      self$properties = assertCharacter(properties, any.missing = FALSE, unique = TRUE)
      self$train = assertFunction(train, args = c("task", "row.ids"), ordered = TRUE)
      self$predict = assertFunction(predict, args = c("model", "task", "row.ids"), ordered = TRUE)
      self$model.extractors = lapply(model.extractors,
        function(m) assertFunction(m, args = c("model", "task", "row.ids"), ordered = TRUE, null.ok = TRUE))
      private$pt = assertChoice(predict.type, capabilities$predict.types[[self$task.type]], fmatch = TRUE)

      # set environments for functions
      if (length(self$model.extractors) > 0)
        for (i in seq_along(self$model.extractors))
          environment(self$model.extractors[[i]]) = environment(self$initialize)
      environment(self$train) = environment(self$predict) = environment(self$initialize)
    },

    print = function(...) {
     catf("Learner '%s' for %s", self$id, self$task.type)
     catf("Predict type: %s", self$predict.type)
     catf(stri_list("Properties: ", self$properties))
    }
  ),
  active = list(
    par.vals = function(rhs) {
      if (missing(rhs))
        return(private$pv)
      assertList(rhs, names = "unique")
      assertSubset(names(rhs), self$par.set$ids)
      private$pv[names(rhs)] = rhs
    },

    predict.type = function(rhs) {
      if (missing(rhs))
        return(private$pt)
      assertChoice(rhs, capabilities$predict.types[[self$task.type]], fmatch = TRUE)
      private$pt = rhs
    }
  ),
  private = list(
    pv = NULL,
    pt = NULL
  )
)

#' @include Dictionary.R
DictionaryLearners = R6Class("DictionaryLearners", inherit = Dictionary,
  public = list(
    initialize = function() {
      super$initialize("Learner")
    }
  )
)

#' @title Registered Learners
#' @docType class
#' @format \code{\link{R6Class}} object
#'
#' @description
#' \code{mlr.learners} is a \code{\link{Dictionary}} used to manage learners.
#'
#' @export
#' @examples
#' mlr.learners$ids
#' mlr.learners$contains("classif.dummy")
#' mlr.learners$get("classif.dummy")
mlr.learners = DictionaryLearners$new()

assertLearner = function(learner, task = NULL) {
  assertR6(learner, "Learner")
  if (!is.null(task)) {
    if (!identical(task$task.type, learner$task.type)) {
      stopf("Learner '%s' (type: %s) is not compatible with task '%s' (type: %s)",
        learner$id, learner$task.type, task$id, task$type)
    }
  }
  invisible(learner)
}

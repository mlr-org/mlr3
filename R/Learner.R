#' @title Abstract learner class
#'
#' @description
#' Abstraction for learners.
#'
#' Predefined learners are stored in [mlr_learners].
#'
#' @section Usage:
#' ```
#' l = Learner$new(id, packages = character(0), par_set = ParamSet$new(), par_vals = list(), properties = character(0))
#' l$id
#' l$packages
#' l$par_set
#' l$par_vals
#' l$properties
#' l$predict_type
#' l$train(task)
#' l$predict(task, model)
#' l$model
#' ```
#'
#' @section Arguments:
#' * `id` (`character(1)`):
#'   identifier for this object.
#' * `task` ([Task]):
#'   Task to train/predict on.
#' * `model` (any):
#'   Fitted model as returned by `train`.
#'
#' @section Details:
#' `$new()` creates a new object of class [Learner].
#'
#' `$id` (`character(1)`) stores the identifier of the object.
#'
#' `$packages` (`character(1)`) stores the names of required packages.
#'
#' `$par_set()` ([paradox::ParamSet]) describes the available hyperparameter and possible settings.
#'
#' `$par_vals()` (`named list`) stores the list set hyperparameter values.
#'
#' `$properties` (`character()`) is a set of tags which describe the properties of the learner.
#'
#' `$predict_type` (`character(1)`) stores the predict type of the learner,
#'  e.g. "response" or "probability" for classification learners of class [LearnerClassif].
#'
#' `$train()` takes a task and returns a model fitted on all observations.
#'
#' `$predict()` takes a task and the model fitted in `$train()` to return predicted labels.
#'
#' `$model` stores the fitted model. If the learner has not been trained, this is `NULL`.
#'
#' @name Learner
#' @keywords internal
#' @family Learner
NULL

#' @include helper_R6.R
#' @export
Learner = R6Class("Learner",
  public = list(
    id = NULL,
    packages = NULL,
    properties = NULL,
    par_set = NULL,
    model = NULL,

    initialize = function(id, packages = character(0L), par_set = ParamSet$new(), par_vals = list(), properties = character(0L)) {
      self$id = assert_id(id)
      self$packages = assert_packages(packages)
      self$properties = assert_properties(properties)
      self$par_set = assert_par_set(par_set)
      private$.par_vals = assert_par_vals(par_vals, par_set)
    },

    train = method_not_implemented,
    test = method_not_implemented,
    print = function(...) {
     catf("Learner '%s' for %s", self$id, self$task_type)
     catf(stri_list("Properties: ", self$properties))
    }
  ),

  active = list(
    par_vals = function(rhs) {
      if (missing(rhs))
        return(private$.par_vals)
      private$.par_vals = assert_par_vals(par_vals, self$par_set)
    },

    predict_type = function(rhs) {
      if (missing(rhs))
        return(private$.predict_type)
      assert_choice(rhs, capabilities$predict_types[[self$task_type]])
      private$.predict_type = rhs
    }
  ),
  private = list(
    .par_vals = NULL,
    .predict_type = NULL
  )
)

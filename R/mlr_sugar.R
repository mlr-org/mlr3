#' @title Syntactic Sugar for Object Construction
#'
#' @name mlr_sugar
#' @description
#' Currently, there are three mechanisms in place to ease the construction of objects stored in a [mlr3misc::Dictionary].
#'
#' 1. Whenever an specific object has to passed or created, it is possible to just pass a scalar string.
#'    This string is used as key to retrieve the object form the respective dictionary.
#'
#' 2. Functions to retrieve objects, set hyperparameters and assign to fields in one go (c.f. [mlr3misc::dictionary_sugar()]):
#'     * `tsk()` for a [Task] from [mlr_tasks].
#'     * `tgen()` for a [TaskGenerator] from [mlr_task_generators].
#'     * `lrn()` for a [Learner] from [mlr_learners].
#'     * `rsmp()` for a [Resampling] from [mlr_resamplings].
#'     * `msr()` for a [Measure] from [mlr_measures].
#'
#' 3. Each task type has an associated default [Measure] (stored in [mlr_reflections]) which is used if no measure is provided.
#'    Classification tasks default to the classification error in ["classif.ce"][mlr_measures_classif.ce],
#'    regression tasks to the mean squared error in ["regr.mse"][mlr_measures_regr.mse].
#'
#' @param .key :: `character(1)`\cr
#'   Key passed to the respective [mlr3misc::Dictionary] to retrieve the object.
#' @param ... :: named `list()`\cr
#'   Parameter values are added to the [paradox::ParamSet] of the object if applicable.
#'   Additional parameters are assigned to the respective field of the object.
#'
#' @return [R6::R6Class] of the respective type.
#' @examples
#' task = tsk("iris", id = "iris2")
#' print(task)
#'
#' learner = lrn("classif.rpart", cp = 0.1, predict_type = "prob")
#' print(learner)
NULL

#' @rdname mlr_sugar
#' @export
tsk = function(.key, ...) {
  dictionary_sugar(mlr_tasks, .key, ...)
}

#' @rdname mlr_sugar
#' @export
tgen = function(.key, ...) {
  dictionary_sugar(mlr_task_generators, .key, ...)
}

#' @rdname mlr_sugar
#' @export
lrn = function(.key, ...) {
  dictionary_sugar(mlr_learners, .key, ...)
}

#' @rdname mlr_sugar
#' @export
rsmp = function(.key, ...) {
  dictionary_sugar(mlr_resamplings, .key, ...)
}

#' @rdname mlr_sugar
#' @export
msr = function(.key, ...) {
  dictionary_sugar(mlr_measures, .key, ...)
}

#' @title Syntactic Sugar for Object Construction
#'
#' @name mlr_sugar
#' @description
#' Functions to retrieve objects, set hyperparameters and assign to fields in one go.
#' Relies on [mlr3misc::dictionary_sugar()] to extract objects from the respective [Dictionary]:
#'
#' * `tsk()` for a [Task] from [mlr_tasks].
#' * `tgen()` for a [TaskGenerator] from [mlr_task_generators].
#' * `lrn()` for a [Learner] from [mlr_learners].
#' * `rsmp()` for a [Resampling] from [mlr_resamplings].
#' * `msr()` for a [Measure] from [mlr_measures].
#'
#' @param .key :: `character(1)`\cr
#'   Key passed to the respective [mlr3misc::Dictionary] to retrieve the object.
#' @param ... :: named `list()`\cr
#'   Named arguments passed to the constructor, to be set as parameters in the [paradox::ParamSet], or to be set as public field.
#'   See [mlr3misc::dictionary_sugar()] for more details.
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

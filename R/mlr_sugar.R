#' @title Syntactic Sugar for Object Construction
#'
#' @name mlr_sugar
#' @description
#' Functions to retrieve objects, set hyperparameters and assign to fields in one go.
#' Relies on [mlr3misc::dictionary_sugar_get()] to extract objects from the respective [mlr3misc::Dictionary]:
#'
#' * `tsk()` for a [Task] from [mlr_tasks].
#' * `tsks()` for a list of [Task]s from [mlr_tasks].
#' * `tgen()` for a [TaskGenerator] from [mlr_task_generators].
#' * `tgens()` for a list of [TaskGenerator]s from [mlr_task_generators].
#' * `lrn()` for a [Learner] from [mlr_learners].
#' * `lrns()` for a list of [Learner]s from [mlr_learners].
#' * `rsmp()` for a [Resampling] from [mlr_resamplings].
#' * `rsmps()` for a list of [Resampling]s from [mlr_resamplings].
#' * `msr()` for a [Measure] from [mlr_measures].
#' * `msrs()` for a list of [Measure]s from [mlr_measures].
#'
#' @param .key (`character(1)`)\cr
#'   Key passed to the respective [mlr3misc::Dictionary] to retrieve the object.
#' @param .keys (`character()`)\cr
#'   Keys passed to the respective [mlr3misc::Dictionary] to retrieve multiple objects.
#' @param ... (named `list()`)\cr
#'   Named arguments passed to the constructor, to be set as parameters in the [paradox::ParamSet], or to be set as public field.
#'   See [mlr3misc::dictionary_sugar_get()] for more details.
#'
#' @return [R6::R6Class] object of the respective type,
#'   or a list of [R6::R6Class] objects for the plural versions.
#' @examples
#' # iris task with new id
#' tsk("iris", id = "iris2")
#'
#' # classification tree with different hyperparameters
#' # and predict type set to predict probabilities
#' lrn("classif.rpart", cp = 0.1, predict_type = "prob")
#'
#' # multiple learners with predict type 'prob'
#' lapply(c("classif.featureless", "classif.rpart"), lrn, predict_type = "prob")
NULL

#' @rdname mlr_sugar
#' @export
tsk = function(.key, ...) {
  dictionary_sugar_get(mlr_tasks, .key, ...)
}

#' @rdname mlr_sugar
#' @export
tsks = function(.keys, ...) {
  dictionary_sugar_mget(mlr_tasks, .keys, ...)
}

#' @rdname mlr_sugar
#' @export
tgen = function(.key, ...) {
  dictionary_sugar_get(mlr_task_generators, .key, ...)
}

#' @rdname mlr_sugar
#' @export
tgens = function(.keys, ...) {
  dictionary_sugar_mget(mlr_task_generators, .keys, ...)
}

#' @rdname mlr_sugar
#' @export
lrn = function(.key, ...) {
  dictionary_sugar_get(mlr_learners, .key, ...)
}

#' @rdname mlr_sugar
#' @export
lrns = function(.keys, ...) {
  dictionary_sugar_mget(mlr_learners, .keys, ...)
}

#' @rdname mlr_sugar
#' @export
rsmp = function(.key, ...) {
  dictionary_sugar_get(mlr_resamplings, .key, ...)
}

#' @rdname mlr_sugar
#' @export
rsmps = function(.keys, ...) {
  dictionary_sugar_mget(mlr_resamplings, .keys, ...)
}

#' @rdname mlr_sugar
#' @export
msr = function(.key, ...) {
  dictionary_sugar_get(mlr_measures, .key, ...)
}

#' @rdname mlr_sugar
#' @export
msrs = function(.keys, ...) {
  dictionary_sugar_mget(mlr_measures, .keys, ...)
}

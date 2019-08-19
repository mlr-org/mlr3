#' @title Syntactic Sugar for Object Construction
#'
#' @name mlr_sugar
#' @description
#' Currently, there are three mechanisms in place to ease the construction of objects.
#'
#' 1. Instead of creating and passing an object manually, it is often possible to pass a character vector.
#'    The elements of this vector are used as keys to retrieve objects form the respective dictionaries.
#'    E.g., `resample(task = "iris", learner = "classif.rpart", resampling = "cv3")` automatically fetches the following:
#'    * The [iris task][mlr_tasks_iris] from [mlr_tasks]
#'    * A [classification decision tree][mlr_learners_classif.rpart] from [mlr_learners]
#'    * A [3-fold cross validation][mlr_resamplings_cv3] from [mlr_resamplings]
#'
#'
#' 2. Functions to retrieve objects, set hyperparameters and assign to fields in one go (c.f. [mlr3misc::dictionary_sugar()]):
#'     * `tsk()` for a [Task] from [mlr_tasks].
#'     * `tgen()` for a [TaskGenerator] from [mlr_task_generators].
#'     * `lrn()` for a [Learner] from [mlr_learners].
#'     * `rsmp()` for a [Resampling] from [mlr_resamplings].
#'     * `msr()` for a [Measure] from [mlr_measures].
#'
#' 3. Each task type has an associated default [Measure] which is used if no measure is provided (i.e., the measure is `NULL`).
#'    This is done internally via call to [default_measures()]
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

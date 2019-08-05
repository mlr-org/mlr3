#' @title Syntactic Sugar for Object Construction
#'
#' @name mlr_sugar
#' @description
#' Currently, there are three mechanisms in place to ease the construction of objects stored in a [mlr3misc::Dictionary].
#'
#' 1. Whenever an specific object has to passed or created, it is possible to just pass a scalar string.
#'    This string is used as key to retrieve the object form the respective dictionary.
#'
#' 2. Functions to retrieve objects, set hyperparameters and assign to fields in one go:
#'     * `tsk()` for a [Task] from [mlr_tasks].
#'     * `gen()` for a [Generator] from [mlr_generators].
#'     * `lrn()` for a [Learner] from [mlr_learners].
#'     * `rsp()` for a [Resampling] from [mlr_resamplings].
#'     * `mea()` for a [Measure] from [mlr_measures].
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

get_from_dict = function(dict, .key, ...) {
  x = dict$get(.key)
  ddd = assert_list(list(...), names = "unique")

  if (length(ddd)) {
    if (exists("param_set", envir = x, inherits = FALSE)) {
      pn = intersect(x$param_set$ids(), names(ddd))
      #TODO: insert or overwrite parameters?
      x$param_set$values = insert_named(x$param_set$values, ddd[pn])
      ddd = remove_named(ddd, pn)
    }

    for (i in seq_along(ddd)) {
      x[[names(ddd)[i]]] = ddd[[i]]
    }
  }

  x
}


#' @rdname mlr_sugar
#' @export
tsk = function(.key, ...) {
  get_from_dict(mlr_tasks, .key, ...)
}

#' @rdname mlr_sugar
#' @export
gen = function(.key, ...) {
  get_from_dict(mlr_generators, .key, ...)
}

#' @rdname mlr_sugar
#' @export
lrn = function(.key, ...) {
  get_from_dict(mlr_learners, .key, ...)
}

#' @rdname mlr_sugar
#' @export
rsp = function(.key, ...) {
  get_from_dict(mlr_resamplings, .key, ...)
}

#' @rdname mlr_sugar
#' @export
mea = function(.key, ...) {
  get_from_dict(mlr_measures, .key, ...)
}

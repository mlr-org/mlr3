#' @section Syntactic Sugar:
#'
#' The mlr3 package provides some shortcuts to ease the creation of its objects.
#'
#' First, instead of the objects themselves, it is possible to pass a `character()` vector which is used to lookup the provided keys in a [mlr3misc::Dictionary]:
#' * [Task] in [mlr_tasks].
#' * [Learner] in [mlr_learners].
#' * [Resampling] in [mlr_resamplings].
#' * [Measure] in [mlr_measures].
#'
#' Additionally, each task has an associated default measure (stored in [mlr_reflections]) which is used as a fallback if no other measure is provided.
#' Classification tasks default to the classification error in ["classif.ce"][mlr_measures_classif.ce], regression tasks to the mean squared error in ["regr.mse"][mlr_measures_regr.mse].

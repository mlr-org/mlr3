#' @title (Un)bundle a Learner
#'
#' @name bundling
#'
#' @description
#' Bundling is the process of processing the model of a trained [`Learner`] so it an be successfully serialized and
#' deserialized. The naming is inspired from \CRANpkg{bundle}.
#'
#' The function:
#' * `learner_bundle(learner)`:
#'   Replaces the learner's model with the bundled model.
#' * `learner_unbundle(learner) :
#'   Replaces the learner's model with the unbundled model.
#' * `learner_bundled(learner)`:
#'   returns `FALSE` if the learner is either not trained or not bundled, otherwise `TRUE`.
#'
#' All three functions are primarily intended to be used when implementing bundling for a [`Learner`].
#' Users who want to (un)bundle a [`Learner`] can instead use the public methods `$bundle()` and `$unbundle()` or the
#' field `$bundled`, which is more in line with `mlr3`'s object oriented design.
#'
#' @section Implementing Bundling for a Learner:
#' In order to implement bundling for a [`Learner`], you need to add:
#' * the public methods `$bundle()` and `$unbundle()`, where you call `learner_bundle(self)` and
#'   `learner_unbundle(self)` respectively.
#' * the active binding `$bundled`, where you simply call `learner_bundled(self)`.
#' * the private method `$.bundle(model)`, which takes in a [`Learner`]'s model and returns it in bundled form,
#'   without modifying the learner's state. Must not depend on the learner's state.
#' * the private method `$.unbundle(model)`, which takes in a [`Learner`]'s bundled model and returns it in
#'   unbundled form. Must not depend on the learner's state.
#'
#' To test the bundling implementation, you can use the internal test helper `expect_bundleable()`.
#' This is also run in `expect_learner()` when a task is provided.
#'
#' For a concrete example on how to implement bundling, see the `LearnerTorch` class from
#' [mlr3torch](https://github.com/mlr-org/mlr3torch).
#'
#' @param learner [`Learner`]\cr
#'   The learner to bundle.
#' @keywords internal
#' @export
learner_unbundle = function(learner) {
  if (is.null(learner$model)) {
    stopf("Cannot unbundle, Learner '%s' has not been trained yet", learner$id)
  }
  if (isFALSE(learner$bundled)) {
    warningf("Learner '%s' has not been bundled, skipping.", learner$id)
  } else if (isTRUE(learner$bundled)) {
    learner$model = get_private(learner)$.unbundle(learner$model)
    learner$state$bundled = FALSE
  }
  invisible(learner)
}

#' @rdname bundling
#' @export
learner_bundle = function(learner) {
  if (is.null(learner$model)) {
    stopf("Cannot bundle, Learner '%s' has not been trained yet", learner$id)
  }
  if (isTRUE(learner$bundled)) {
    warningf("Learner '%s' has already been bundled, skipping.", learner$id)
  } else if ("bundle" %in% learner$properties) {
    learner$model = get_private(learner)$.bundle(learner$model)
    learner$state$bundled = TRUE
  }
  invisible(learner)
}

#' @rdname bundling
#' @export
learner_bundled = function(learner) {
  isTRUE(learner$state$bundled) && !is.null(learner$model)
}

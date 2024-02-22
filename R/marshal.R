#' @title (Un)marshal a Learner
#'
#' @name marshaling
#'
#' @description
#' marshaling is the process of processing the model of a trained [`Learner`] so it an be successfully serialized and
#' deserialized. The naming is inspired by the [marshal package](https://github.com/HenrikBengtsson/marshal) and we
#' plan to fully migrate to this package once it is on CRAN.
#' The supported implementation until then should therfore be considered as a temporary solution and is likely
#' to change in the future.
#'
#' The central functions (and the only methods that are used by `mlr3` internally) are:
#' * the S3 generic `marshal_model(model, ...)`.
#'   Which takes in a model and returns it in marshaled form.
#'   The marshaled object should be a list with named elements `marshaled` and `packages`, where the former contains
#'   the actual marshaled object, and the latter the packages required to unmarshal it.
#'   This list should have as classes the classes of the original object with the suffix `"_marshaled"` added and the
#'   root class should be set to `"marshaled"`.
#' * the S3 generic `unmarshal_model(model, ...)`.
#'   Which takes in the marshaled model and returns it in unmarshaled form.
#'   The generic takes care that the packages specified during `"marshal"` are loaded, and errs if they are not.
#'   The returned object must not inherit from class `"marshaled"`.
#' * the function `marshaled_model(model)`, which returns `TRUE` if the model inherits from class `"marshaled"`
#'   and `FALSE` otherwise.
#'
#' In order to implement marshaling for a Learner, you only need to overload the `marshal_model` and `unmarshal_model`
#' methods and tag the learner with the `"marshal"` property accordingly.
#'
#' To make marshaling accessible in an R6-manner, you should also add the public methods `$marshal()`, `$unmarshal()`
#' and the active binding `$marshaled`.
#' To make this as convenient as possible, the functions `learner_marshal(learner)`, `learner_unmarshal(learner)`
#' and `learner_marshaled(learner)` are provided and can be called from within the public methods.
#' All three functions throw an error if the learner is not trained and otherwise call
#' `marshal_model()`, `unmarshal_model()` or `marshaled_model()` on the learner's model.
#'
#' You can verify whether you have correctly implemented marshaling by using the internal test helper
#' `expect_marshalable_learner()`. This is also run by `expect_learner()` if a task is provided.
#'
#' For a concrete example on how to implement marshaling, see [`LearnerClassifLily`].
#'
#' @param learner [`Learner`]\cr
#'   The learner.
#' @keywords internal
#' @export
learner_unmarshal = function(learner) {
  # no need to check for 'marshal' property as this method should only be available for such learners
  if (is.null(learner$model)) {
    stopf("Cannot unmarshal, Learner '%s' has not been trained yet", learner$id)
  }
  # this will do nothing if the model was not marshaled
  learner$model = unmarshal_model(learner$model)
  invisible(learner)
}

#' @rdname marshaling
#' @export
learner_marshal = function(learner) {
  # no need to check for 'marshal' property as this method should only be available for such learners
  if (is.null(learner$model)) {
    stopf("Cannot marshal, Learner '%s' has not been trained yet", learner$id)
  }
  # this will do nothing if the model was already marshaled
  learner$model = marshal_model(learner$model)
  invisible(learner)
}

#' @rdname marshaling
#' @export
learner_marshaled = function(learner) {
  # no need to check for 'marshal' property as this method should only be available for such learners
  if (is.null(learner$model)) {
    stopf("Cannot check marshaled status, Learner '%s' has not been trained yet", learner$id)
  }
  marshaled_model(learner$model)
}

#' @rdname marshaling
#' @export
marshal_model = function(model, clone, ...) {
  UseMethod("marshal_model")
}

#' @rdname marshaling
#' @export
unmarshal_model = function(model, clone, ...) {
  if (marshaled_model(model) && is.character(model$packages)) {
    require_namespaces(model$packages)
  }
  UseMethod("unmarshal_model")
}

#' @rdname marshaling
#' @export
marshaled_model = function(model) {
  test_class(model, "marshaled")
}

#' @export
marshal_model.default = function(model, ...) {
  classes = class(model)
  structure(list(marshaled = model), class = c(paste0(classes, "_marshaled"), "marshaled"))
}

#' @export
marshal_model.marshaled = function(model, ...) {
  model
}

#' @export
unmarshal_model.marshaled = function(model, ...) {
  model[["marshaled"]]
}

#' @export
unmarshal_model.default = function(model, ...) {
  model
}

marshal_state_if_model = function(state, ...) {
  if (!is.null(state$model)) {
    state$model = marshal_model(state$model)
  }
  state
}

unmarshal_state_if_model = function(state, ...) {
  if (!is.null(state$model)) {
    state$model = unmarshal_model(state$model)
  }
  state
}

#' @title (Un)marshal a Learner
#'
#' @name marshaling
#'
#' @description
#' Marshaling is the process of processing the model of a trained [`Learner`] so it an be successfully serialized and
#' deserialized. The naming is inspired by the [marshal package](https://github.com/HenrikBengtsson/marshal) and we
#' plan to fully migrate to this package once it is on CRAN.
#' The supported implementation until then should therfore be considered as a temporary solution and is likely
#' to change in the future.
#'
#' The central functions (and the only methods that are used by `mlr3` internally) are:
#' * the S3 generic `marshal_model(model, inplace, ...)`.
#'   Which takes in a model and returns it in marshaled form.
#'   The marshaled object should be a list with named elements `marshaled` and `packages`, where the former contains
#'   the marshaled object, and the latter the packages required to unmarshal it.
#'   This list should have the classes of the original object with the suffix `"_marshaled"` appended and the
#'   root class should be set to `"marshaled"`.
#' * the S3 generic `unmarshal_model(model, inplace ...)`.
#'   Which takes in the marshaled model and returns it in unmarshaled form.
#'   The generic takes care that the packages specified during `"marshal"` are loaded, and errs if they are not.
#'   The returned object must not inherit from class `"marshaled"`.
#' * the function `is_ marshaled_model(model)`, which returns `TRUE` if the model inherits from class `"marshaled"`
#'   and `FALSE` otherwise.
#'
#'
#' The contract of these functions is:
#' * `unmarshal_model(marshal_model(x))` returns `x` as is.
#' * If `is_marshaled_model(x)` is `TRUE`, this means that `x` is in marshaled form.
#'   Note that it is not guarateed that `is_marshaled_model(marshal_model(x))` returns `TRUE`.
#'   This is because the default `marshal_model(x)` returns `x` as-is.
#'
#'
#' @section Implementing Marshaling:
#'
#' In order to implement marshaling for a Learner, you only need to overload the `marshal_model` and `unmarshal_model`
#' methods and tag the learner with the `"marshal"` property accordingly.
#'
#' To make marshaling accessible in an R6-manner, you should also add the public methods `$marshal()`, `$unmarshal()`
#' and the active binding `$marshaled`.
#' To make this as convenient as possible, the functions `learner_marshal(learner)`, `learner_unmarshal(learner)`
#' and `learner_marshaled(learner)` are provided and can be called from within the public methods.
#' All three functions throw an error if the learner is not trained and otherwise call
#' `marshal_model()`, `unmarshal_model()` or `is_marshaled_model()` on the learner's model.
#'
#' You can verify whether you have correctly implemented marshaling by using the internal test helper
#' `expect_marshalable_learner()`. This is also run by `expect_learner()` if a task is provided.
#'
#' For a concrete example on how to implement marshaling, see [`LearnerRegrDebug`].
#'
#' @param .learner [`Learner`]\cr
#'   The learner.
#' @param inplace (`logical(1)`)\cr
#'   Whether to do the (un)marshaling in-place.
#'   Only relevant for objects with reference semantics.
#'   Set this to `TRUE` if you don't intend to keep the original object, e.g. in functions that return
#'   a marshaled object. The default of methdos should always be `FALSE`, which should leave the original object
#'   as-is.
#' @param ... (any)\cr
#'   Additional parameters, currently unused.
#'
#' @keywords internal
#' @export
learner_unmarshal = function(.learner, ...) {
  # no need to check for 'marshal' property as this method should only be available for such learners
  if (is.null(.learner$model)) {
    stopf("Cannot unmarshal, Learner '%s' has not been trained yet", .learner$id)
  }
  # this will do nothing if the model was not marshaled
  .learner$model = unmarshal_model(model = .learner$model, inplace = TRUE, ...)
  invisible(.learner)
}

#' @rdname marshaling
#' @export
learner_marshal = function(.learner, ...) {
  # no need to check for 'marshal' property as this method should only be available for such learners
  if (is.null(.learner$model)) {
    stopf("Cannot marshal, Learner '%s' has not been trained yet", .learner$id)
  }
  # this will do nothing if the model was already marshaled
  .learner$model = marshal_model(model = .learner$model, inplace = TRUE, ...)
  invisible(.learner)
}

#' @rdname marshaling
#' @export
learner_marshaled = function(.learner) {
  # no need to check for 'marshal' property as this method should only be available for such learners
  if (is.null(.learner$model)) {
    stopf("Cannot check marshaled status, Learner '%s' has not been trained yet", .learner$id)
  }
  is_marshaled_model(model = .learner$model)
}

#' @rdname marshaling
#' @export
marshal_model = function(model, inplace = FALSE, ...) {
  assert_flag(inplace)
  UseMethod("marshal_model")
}

#' @rdname marshaling
#' @export
unmarshal_model = function(model, inplace = FALSE, ...) {
  if (is_marshaled_model(model) && is.character(model$packages)) {
    require_namespaces(model$packages)
  }
  assert_flag(inplace)
  UseMethod("unmarshal_model")
}

#' @rdname marshaling
#' @export
is_marshaled_model = function(model) {
  test_class(model, "marshaled")
}

#' @export
marshal_model.default = function(model, ...) {
  model
}

#' @export
unmarshal_model.marshaled = function(model, inplace = FALSE,...) {
  model[["marshaled"]]
}

#' @export
unmarshal_model.default = function(model, inplace = FALSE, ...) {
  model
}

marshal_state_if_model = function(.state, ...) {
  if (!is.null(.state$model)) {
    .state$model = marshal_model(.state$model, ...)
  }
  .state
}

unmarshal_state_if_model = function(.state, ...) {
  if (!is.null(.state$model)) {
    .state$model = unmarshal_model(model = .state$model, ...)
  }
  .state
}

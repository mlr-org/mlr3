#' @title (Un)marshal a Learner
#'
#' @name marshaling
#'
#' @description
#' Marshaling is the process of processing the model of a trained [`Learner`] so it an be successfully serialized and
#' deserialized. The naming is inspired by the [marshal package](https://github.com/futureverse/marshal) and we
#' plan to fully migrate to this package once it is on CRAN.
#' The current implementation should therfore be considered as a temporary solution and is likely
#' to change in the future.
#'
#' The central functions (and the only methods that are used by `mlr3` internally) are:
#' * the S3 generic `marshal_model(model, inplace, ...)`.
#'   Which takes in a model and returns it in marshaled form.
#'   This means, that the resulting object can be serialized and de-serialzed without loss of information.
#'   If a model is serializable anyway, nothing has to be implemented and the generic will fall back to the
#'   default implementation of `marshal_model`, which is to return the object as-is.
#'   Otherwise, the marshaled object should be a list with named elements `marshaled` and `packages`, where the former contains
#'   the marshaled object, and the latter the package that contains the packages required to unmarshal.
#'   Most importantly, this list should contain the package that contains the `unmarshal_model` method.
#'   The returned object should have the classes of the original object with the suffix `"_marshaled"` appended and the
#'   root class should be set to `"marshaled"`.
#' * the S3 generic `unmarshal_model(model, inplace ...)`.
#'   Which takes in the marshaled model and returns it in unmarshaled form.
#'   The generic takes care that the packages specified during `"marshal"` are loaded, and errs if they are not availabe.
#'   Calling this function on a marshaled model should reconstruct the original model, i.e.
#'   `unmarshal_model(marshal_model(x))` should return `x`.
#'   The default implementation of this generic returns `x` as-is.
#' * the function `is_marshaled_model(model)`.
#'   This (helper) function returns `TRUE` if the model inherits from class `"marshaled"` and `FALSE` otherwise.
#'   Note that it is not guarateed that `is_marshaled_model(marshal_model(x))` returns `TRUE`.
#'   This is because the default `marshal_model(x)` returns `x` as-is.
#'
#' For both `marshal_model` and `unmarshal_model`, the `inplace` argument determines whether in-place marshaling
#' should be performed. This is especially relevant in the context of references semantics.
#' If `inplace` is `FALSE`, the original input should not be modified, otherwise this is allowed.
#' Note that the input and output can still share references, even when `inplace` is `FALSE`.
#'
#' @section Implementing Marshaling:
#'
#' In order to implement marshaling for a Learner, you need to overload the `marshal_model` and `unmarshal_model`
#' methods for the class of the learner's model and tag the learner with the `"marshal"` property.
#' To make marshaling accessible in an R6-manner, you should also add the public methods `$marshal()`, `$unmarshal()`
#' and the active binding `$marshaled`.
#' To make this as convenient as possible, the functions `learner_marshal(.learner, ...)`, `learner_unmarshal(.learner, ...)`
#' and `learner_marshaled(.learner)` are provided and can be called from the public methods.
#'
#' You can verify whether you have correctly implemented marshaling by using the internal test helper
#' `expect_marshalable_learner(learner, task)`. This is also run by `expect_learner()` if a task is provided.
#'
#' For a concrete example on how to implement marshaling, see [`LearnerClassifDebug`].
#'
#' @param .learner [`Learner`]\cr
#'   The learner.
#' @param ... (any)\cr
#'   Additional parameters, currently unused.
#' @param model (any)\cr
#'   Model to marshal.
#' @param inplace (`logical(1)`)\cr
#'   Whether to marshal in-place.
#'
#' @export
learner_unmarshal = function(.learner, ...) {
  # this will do nothing if the model was not marshaled
  .learner$model = unmarshal_model(model = .learner$model, inplace = TRUE, ...)
  invisible(.learner)
}

#' @rdname marshaling
#' @export
learner_marshal = function(.learner, ...) {
  # this will do nothing if the model was already marshaled
  .learner$model = marshal_model(model = .learner$model, inplace = TRUE, ...)
  invisible(.learner)
}

#' @rdname marshaling
#' @export
learner_marshaled = function(.learner) {
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
  inherits(model, "marshaled")
}

#' @export
marshal_model.default = function(model, ...) {
  model
}

#' @export
unmarshal_model.default = function(model, inplace = FALSE, ...) {
  model
}

marshal_state_if_model = function(.state, inplace, ...) {
  if (!is.null(.state$model)) {
    .state$model = marshal_model(.state$model, inplace, ...)
  }
  .state
}

unmarshal_state_if_model = function(.state, inplace, ...) {
  if (!is.null(.state$model)) {
    .state$model = unmarshal_model(model = .state$model, inplace = inplace, ...)
  }
  .state
}

#' @export
print.marshaled = function(x, ...) {
  catn(sprintf("<%s>", class(x)[[1L]]))
}

#' @title (Un)marshal a Learner
#'
#' @name marshalling
#'
#' @description
#' Marshalling is the process of processing the model of a trained [`Learner`] so it an be successfully serialized and
#' deserialized. The naming is inspired from package [marshall](https://github.com/HenrikBengtsson/marshal) and we
#' plan to fully migrate to this package once it is on CRAN.
#' The supported implementation until then should therfore be considered as a temporary solution and is likely
#' to change in the future.
#'
#' The central functions (and the only methods that are used by `mlr3` internally) are:
#' * the S3 generic `marshal_model(model, ...)`.
#'   Which takes in a model and returns it in marshalled form.
#'   The suffix `"_marshalled"` should be added to the class of the returned object and the root class must
#'   be set to `"marshalled"`.
#' * the S3 generic `unmarshal_model(model, ...)`.
#'   Which takes in a model and returns it in unmarshalled form.
#'   The returned object must not inherit from class `"marshalled"`.
#' * the function `marshalled_model(model)`, which returns `TRUE` if the model inherits from class `"marshalled"`
#'   and `FALSE` otherwise.
#'
#' In order to implement marshalling for a Learner, you only need to overload the `marshal_model` and `unmarshal_model`
#' methods and tag the learner with the `"marshal"` property accordingly.
#'
#' To make marshalling accessible in an R6-manner, you should also add the public methods `$marshal()`, `$unmarshal()`
#' and the active binding `$marshalled`.
#' To make this as convenient as possible, the functions `learner_marshal(learner)`, `learner_unmarshal(learner)`
#' and `learner_marshalled(learner)` are provided and can be called from within the public methods.
#' All three functions throw an error if the learner is not trained and otherwise call
#' `marshal_model()`, `unmarshal_model()` or `marshalled_model()` on the learner's model.
#'
#' You can verify whether you have correctly implemented marshalling by using the internal test helper
#' `expect_marshallable_learner()`. This is also run by `expect_learner()` if a task is provided.
#'
#' For a concrete example on how to implement marshalling, see [`LearnerClassifLily`].
#'
#' @param learner [`Learner`]\cr
#'   The learner to marshal.
#' @keywords internal
#' @export
learner_unmarshal = function(learner) {
  if (is.null(learner$model)) {
    stopf("Cannot unmarshal, Learner '%s' has not been trained yet", learner$id)
  }
  learner$model = unmarshal_model(learner$model)
  invisible(learner)
}

#' @rdname marshalling
#' @export
learner_marshal = function(learner) {
  if (is.null(learner$model)) {
    stopf("Cannot marshal, Learner '%s' has not been trained yet", learner$id)
  }
  learner$model = marshal_model(learner$model)
  invisible(learner)
}

#' @rdname marshalling
#' @export
learner_marshalled = function(learner) {
  if (is.null(learner$model)) {
    stopf("Cannot check marshalled status, Learner '%s' has not been trained yet", learner$id)
  }
  marshalled_model(learner$model)
}

#' @rdname marshalling
#' @export
marshal_model = function(model, ...) {
  UseMethod("marshal_model")
}

#' @rdname marshalling
#' @export
unmarshal_model = function(model, ...) {
  UseMethod("unmarshal_model")
}

#' @rdname marshalling
#' @export
marshalled_model = function(model) {
  test_class(model, "marshalled")
}

#' @export
marshal_model.default = function(model, ...) {
  model
}

#' @export
unmarshal_model.default = function(model, ...) {
  model
}

marshal_state = function(state, ...) {
  state$model = marshal_model(state$model)
  state
}

unmarshal_state = function(state, ...) {
  state$model = unmarshal_model(state$model)
  state
}

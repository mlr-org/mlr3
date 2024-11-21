#' @title Create Workhorse Callback
#'
#' @description
#' Callbacks allow to customize the behavior of processes in mlr3.
#'
#' @export
CallbackWorkhorse= R6Class("CallbackWorkhorse",
  inherit = Callback,
  public = list(

    on_workhorse_before_train = NULL,

    on_workhorse_before_predict = NULL,

    on_workhorse_before_result = NULL
  )
)

#' @title Create Workhorse Callback
#'
#' @description
#' Function to create a [CallbackWorkhorse].
#'
#' ```
#' Start Workhorse
#'  - on_workhorse_before_train
#'  - on_workhorse_before_predict
#'  - on_workhorse_before_result
#' End Tuning
#' ```
#
#' @details
#' When implementing a callback, each function must have two arguments named `callback` and `context`.
#' A callback can write data to the state (`$state`), e.g. settings that affect the callback itself.
#' Workhorse callbacks access [ContextWorkhorse].
#'
#' @param id (`character(1)`)\cr
#'  Identifier for the new instance.
#' @param label (`character(1)`)\cr
#'  Label for the new instance.
#' @param man (`character(1)`)\cr
#'  String in the format `[pkg]::[topic]` pointing to a manual page for this object.
#'  The referenced help package can be opened via method `$help()`.
#'
#' @export
#' @inherit CallbackWorkhorse examples
callback_workhorse = function(
  id,
  label = NA_character_,
  man = NA_character_,
  on_workhorse_before_train = NULL,
  on_workhorse_before_predict = NULL,
  on_workhorse_before_result = NULL
  ) {
  stages = discard(set_names(list(
    on_workhorse_before_train,
    on_workhorse_before_predict,
    on_workhorse_before_result),
    c(
      "on_workhorse_before_train",
      "on_workhorse_before_predict",
      "on_workhorse_before_result"
    )), is.null)

  walk(stages, function(stage) assert_function(stage, args = c("callback", "context")))
  callback = CallbackWorkhorse$new(id, label, man)
  iwalk(stages, function(stage, name) callback[[name]] = stage)
  callback
}

#' @title Assertions for Callbacks
#'
#' @description
#' Assertions for [CallbackWorkhorse] class.
#'
#' @param callback ([CallbackWorkhorse]).
#' @param null_ok (`logical(1)`)\cr
#'   If `TRUE`, `NULL` is allowed.
#'
#' @return [CallbackWorkhorse | List of [CallbackWorkhorse]s.
#' @export
assert_workhorse_callback = function(callback, null_ok = FALSE) {
  if (null_ok && is.null(callback)) return(invisible(NULL))
  assert_class(callback, "CallbackWorkhorse")
  invisible(callback)
}

#' @export
#' @param callbacks (list of [CallbackWorkhorse]).
#' @rdname assert_workhorse_callback
assert_workhorse_callbacks = function(callbacks) {
  invisible(lapply(callbacks, assert_workhorse_callback))
}

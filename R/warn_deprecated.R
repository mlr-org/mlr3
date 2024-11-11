
#' @title Give a Warning about a Deprecated Function, Argument, or Active Binding
#'
#' @description
#' Generates a warning when a deprecated function, argument, or active binding
#' is used or accessed. A warning will only be given once per session, and all
#' deprecation warnings can be suppressed by setting the option
#' `mlr3.warn_deprecated = FALSE`.
#'
#' The warning is of the format
#' "what is deprecated and will be removed in the future."
#'
#' Use the 'deprecated_binding()' helper function to create an active binding
#' that generates a warning when accessed.
#' @param what (character(1))\cr
#'   A description of the deprecated entity. This should be somewhat descriptive,
#'   e.g. `"Class$method()"` or `"Argument 'foo' of Class$method()"`.\cr
#'   The `what` is used to determine if the warning has already been given, so
#'   it should be unique for each deprecated entity.
#' @keywords internal
#' @export
warn_deprecated = function(what) {
  assert_string(what)
  if (getOption("mlr3.warn_deprecated", TRUE) && !exists(what, envir = deprecated_warning_given_db)) {
    warning(paste0(what, " is deprecated and will be removed in the future."))
    assign(what, TRUE, envir = deprecated_warning_given_db)
  }
}

deprecated_warning_given_db = new.env(parent = emptyenv())

#' @title Create an Active Binding that Generates a Deprecation Warning
#'
#' @description
#' Creates an active binding that generates a warning when accessed, using
#' `warn_deprecated()`. The active binding will otherwise be read-only.
#'
#' @param what (character(1))\cr
#'   A description of the deprecated binding. Should be of the form `"Class$field"`.
#' @param value (any)\cr
#'   The value of the active binding. This should be an expression that will
#'   be evaluated in the context of the active binding. It could, for example,
#'   refer to `self`.
#' @examples
#' MyClass = R6::R6Class("MyClass", public = list(),
#'   active = list(
#'     foo = deprecated_binding("MyClass$foo", "bar")
#'   )
#' )
#' mco = MyClass$new()
#' mco$foo
#' @keywords internal
#' @export
deprecated_binding = function(what, value) {
  assert_string(what)
  # build the function-expression that should be evaluated in the parent frame.
  fnq = substitute(function(rhs) {
      # don't throw a warning if we are converting the R6-object to a list, e.g.
      # when all.equals()-ing it.
      if (!identical(sys.call(-1)[[1]], quote(as.list.environment))) {
        warn_deprecated(what)
      }
      ## 'value' could be an expression that gets substituted here, which we only want to evaluate once
      x = value
      if (!missing(rhs) && !identical(rhs, x)) {
        stop(sprintf("%s read-only.", what))
      }
      x
    },
    # we substitute the 'what' constant directly, but the 'value' as expression.
    env = list(what = what, value = substitute(value))
  )
  eval.parent(fnq)
}

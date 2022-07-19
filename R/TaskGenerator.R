#' @title TaskGenerator Class
#'
#' @include mlr_reflections.R
#'
#' @description
#' Creates a [Task] of arbitrary size.
#' Predefined task generators are stored in the [dictionary][mlr3misc::Dictionary] [mlr_task_generators],
#' e.g. [`xor`][mlr_task_generators_xor].
#'
#' @template param_id
#' @template param_param_set
#' @template param_task_type
#' @template param_packages
#' @template param_man
#' @template param_label
#'
#' @template seealso_task_generator
#' @export
TaskGenerator = R6Class("TaskGenerator",
  public = list(
    #' @template field_id
    id = NULL,

    #' @template field_label
    label = NULL,

    #' @template field_task_type
    task_type = NULL,

    #' @template field_param_set
    param_set = NULL,

    #' @template field_packages
    packages = NULL,

    #' @template field_man
    man = NULL,

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(id, task_type, packages = character(), param_set = ps(), label = NA_character_, man = NA_character_) {
      self$id = assert_string(id, min.chars = 1L)
      self$param_set = assert_param_set(param_set)
      self$packages = union("mlr3", assert_character(packages, any.missing = FALSE, min.chars = 1L))
      self$task_type = assert_choice(task_type, mlr_reflections$task_generators$type)
      self$label = assert_string(label, na.ok = TRUE)
      self$man = assert_string(man, na.ok = TRUE)

      check_packages_installed(packages, msg = sprintf("Package '%%s' required but not installed for TaskGenerator '%s'", id))
    },

    #' @description
    #' Helper for print outputs.
    format = function() {
      sprintf("<%s:%s>", class(self)[1L], self$id)
    },

    #' @description
    #' Printer.
    #' @param ... (ignored).
    print = function(...) {
      catn(format(self), if (is.null(self$label) || is.na(self$label)) "" else paste0(": ", self$label))
      catn(str_indent("* Task type:", self$task_type))
      catn(str_indent("* Packages:", self$packages))
      catn(str_indent("* Parameters:", as_short_string(self$param_set$values, 1000L)))
      catn(str_indent("* Manual:", sprintf("?%s", self$man)))
    },

    #' @description
    #' Creates a task of type `task_type` with `n` observations, possibly using additional settings stored in `param_set`.
    #'
    #' @param n (`integer(1)`)\cr
    #'   Number of rows to generate.
    #' @return [Task].
    generate = function(n) {
      n = assert_count(n, coerce = TRUE)
      require_namespaces(self$packages)
      private$.generate(n)
    }
  )
)

convert_mlbench = function(obj) {
  y = factor(LETTERS[as.integer(obj$classes)], levels = LETTERS[seq_len(uniqueN(obj$classes))])
  X = set_col_names(obj$x, sprintf("x%i", seq_col(obj$x)))
  insert_named(as.data.table(X), list(y = y))
}

# #' @export
# format_list_item.TaskGenerator = function(x, ...) { # nolint
#   sprintf("<tgen:%s>", x$id)
# }

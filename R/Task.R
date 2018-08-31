#' @title Abstract Tasks
#' @format [R6Class()] object
#'
#' @description
#' This is the abstract base class for task objects.
#' Use [TaskClassif] or [TaskRegr] to construct tasks instead of this class.
#'
#' @template fields-task
#' @template fields-supervisedtask
#'
#' @return [Task].
#' @export
#' @family Tasks
#' @include capabilities.R
#' @examples
#' b = BackendDataTable$new(iris)
#' task = Task$new("iris", backend = b)
#' task$head()
#' task$formula
Task = R6Class("Task",
  cloneable = TRUE,
  public = list(
    id = NULL,
    backend = NULL,
    row_info = NULL,
    col_info = NULL,
    measures = list(),
    order = character(0L),

    initialize = function(id, backend) {
      self$id = assert_string(id, min.chars = 1L)
      self$backend = assert_r6(backend, "Backend")

      cn = backend$colnames
      types = assert_subset(vcapply(backend$head(1L), class), capabilities$task_col_types, fmatch = TRUE)

      self$row_info = data.table(
        id = backend$rownames,
        role = "use",
        key = "id")

      self$col_info = data.table(
        id = cn,
        role = ifelse(cn == backend$primary_key, "primary_key", "feature"),
        type = types[chmatch(cn, names(types), 0L)],
        key = "id"
      )
    },

    print = function(...) {
      catf("Task '%s' of type %s (%i x %i)", self$id, class(self)[1L], self$nrow, self$ncol)
      catf(stri_list("Target: ", self$target_names))
      catf(stri_list("Features: ", stri_peek(self$feature_names)))
      catf(stri_list("Order by: ", self$order))
      catf(stri_list("Public: ", setdiff(ls(self), c("initialize", "print"))))
    },

    data = function(rows = NULL, cols = NULL) {
      if (is.null(rows)) {
        selected_rows = self$row_info[role == "use", "id"][[1L]]
      } else {
        selected_rows = self$row_info[id %in% rows & role == "use", "id"][[1L]]
        # FIXME: check for valid row ids?
      }

       if (is.null(cols)) {
        selected_cols = self$col_info[role %in% c("feature", "target"), "id"][[1L]]
      } else {
        selected_cols = self$col_info[id %in% cols & role %in% c("feature", "target"), "id"][[1L]]
        if (length(selected_cols) != length(cols))
          stopf("Invalid column ids provided")
      }

      extra_cols = character(0L)
      if (length(self$order)) {
        extra_cols = setdiff(self$order, selected_cols)
        selected_cols = union(selected_cols, extra_cols)
      }

      data = self$backend$data(rows = selected_rows, cols = selected_cols)

      if (nrow(data) != length(selected_rows)) {
        stopf("Backend did not return the rows correctly: %i requested, %i received", length(selected_rows), nrow(data))
      }

      if (ncol(data) != length(selected_cols)) {
        stopf("Backend did not return the cols correctly: %i requested, %i received", length(selected_cols), ncol(data))
      }

      if (length(self$order)) {
        setorderv(data, self$order)
      }

      if (length(extra_cols))
        data[, (extra_cols) := NULL]
      return(data)
    },

    head = function(n = 6L) {
      assert_count(n)
      ids = head(self$row_info[role == "use", "id", with = FALSE][[1L]], n)
      self$data(rows = ids, cols = c(self$feature_names, self$target_names))
    },

    row_ids = function(subset = NULL, as.vector = TRUE) {
      if (is.null(subset)) {
        result = self$row_info[role == "use", "id"]
      } else {
        type = attr(subset, "subset_type")
        if (is.null(type)) {
          result = self$row_info[role == "use"][as.integer(subset), "id"]
        } else {
          result = switch(type,
            "ids" = self$row_info[.(subset), nomatch = 0L],
            "numbers" = self$row_info[role == "use"][subset, "id"],
            "roles" = self$row_info[role %in% subset, "id"],
            stopf("Unknown subset_type"))
        }
      }
      attr(result, "subset_type") = "ids"
      if (as.vector) result[[1L]] else result
    }
  ),

  active = list(
    feature_names = function() {
      self$col_info[role == "feature", "id"][[1L]]
    },

    target_names = function() {
      self$col_info[role == "target", "id"][[1L]]
    },

    nrow = function() {
      self$row_info[role == "use", .N]
    },

    ncol = function() {
      self$col_info[role %in% c("feature", "target"), .N]
    },

    col_types = function() {
      self$col_info[role %in% c("feature", "target"), c("id", "type")]
    },

    formula = function() {
      tn = self$target_names
      if (length(tn) == 0L)
        tn = NULL
      reformulate(self$feature_names, response = tn)
    }
  ),

  private = list(
    deep_clone = function(name, value) {
      # NB: Backends are never copied!
      if (name %in% c("row_info", "col_info")) copy(value) else value
    }
  )
)

assert_task = function(task) {
  assert_r6(task, "Task")
}

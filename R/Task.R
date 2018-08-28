#' @title Basic Tasks
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
#' task = Task$new("iris", data = iris)
#' task$formula
Task = R6Class("Task",
  # Base Class for Tasks
  public = list(
    task_type = NA_character_,
    id = NULL,
    backend = NULL,
    row_info = NULL,
    col_info = NULL,
    order = character(0L),
    blocking = character(0L),

    initialize = function(id, data) {
      self$id = assert_string(id, min.chars = 1L)
      if (inherits(data, "Backend")) {
        self$backend = list(data)
      } else {
        assert_data_frame(data)
        self$backend = list(BackendDataTable$new(data = data))
      }

      cn = self$backend[[1L]]$colnames
      types = assert_subset(vcapply(self$backend[[1L]]$head(1L), class), capabilities$task_col_types, fmatch = TRUE)

      self$row_info = data.table(
        id = self$backend[[1L]]$rownames,
        role = "training",
        key = "id")

      self$col_info = data.table(
        id = cn,
        role = ifelse(cn == self$backend[[1L]]$primary_key, "primary_key", "feature"),
        type = types[chmatch(cn, names(types), 0L)],
        key = "id"
      )
    },

    print = function(...) {
      catf("Task '%s' of type %s (%i x %i)", self$id, self$task_type, self$nrow, self$ncol)
      catf(stri_list("Target: ", self$target_names))
      catf(stri_list("Features: ", stri_peek(self$feature_names)))
      catf(stri_list("Order by: ", self$order))
      catf(stri_list("Blocking: ", self$blocking))
      catf(stri_list("Public: ", setdiff(ls(self), c("initialize", "print"))))
    },

    data = function(rows = NULL, cols = NULL, filter_rows = TRUE, filter_cols = TRUE) {
      if (is.null(rows)) {
        selected_rows = if (filter_rows)
          self$row_info[role == "training", "id"][[1L]]
        else
          self$row_info$id
      } else {
        selected_rows = if (filter_rows)
          self$row_info[id %in% rows & role == "training", "id"][[1L]]
        else
          self$row_info[id %in% rows, "id"][[1L]]

        if (length(selected_rows) != length(rows))
          stopf("Invalid row_ids provided")
      }

      if (is.null(cols)) {
        selected_cols = if (filter_cols)
          self$col_info[role %in% c("feature", "target"), "id"][[1L]]
        else
          self$col_info$id
      } else {
        selected_cols = if (filter_cols)
          self$col_info[id %in% cols & role %in% c("feature", "target"), "id"][[1L]]
        else
          self$col_info[id %in% cols, "id"][[1L]]

        if (length(selected_cols) != length(cols))
          stopf("Invalid col ids provided")
      }

      extra_cols = character(0L)
      if (length(self$order)) {
        extra_cols = setdiff(self$order, selected_cols)
        selected_cols = union(selected_cols, extra_cols)
      }

      data = rbindlist(
        lapply(self$backend, function(b) b$data(rows = selected_rows, cols = selected_cols)),
        fill = TRUE)

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
      ids = head(self$row_info[role == "training", "id", with = FALSE][[1L]], n)
      self$data(rows = ids, cols = c(self$feature_names, self$target_names))
    },

    row_ids = function(subset = NULL, as.vector = TRUE) {
      if (is.null(subset)) {
        result = self$row_info[role == "training", "id"]
      } else {
        type = attr(subset, "subset_type")
        if (is.null(type)) {
          result = self$row_info[role == "training"][as.integer(subset), "id"]
        } else {
          result = switch(type,
            "ids" = self$row_info[.(subset), nomatch = 0L],
            "numbers" = self$row_info[role == "training"][subset, "id"],
            "roles" = self$row_info[role %in% subset, "id"],
            stopf("Unknown subset_type"))
        }
      }
      attr(result, "subset_type") = "ids"
      if (as.vector) result[[1L]] else result
    },

    add_backend = function(backend, row.role = "validation") {
      b = self$backend[[1L]]
      assert_names(backend$colnames, subset.of = b$colnames, must.include = b$primary_key)

      rn = backend$rownames
      for (b in self$backend) {
        clashes = b$data(rn, b$primary_key)
        if (nrow(clashes))
          stopf("Cannot add new backend: name clashes with primary_key id: %s", stri_peek(clashes[[1L]]))
      }

      self$backend[[length(self$backend) + 1L]] = backend
      self$row_info = rbind(self$row_info, data.table(id = rn, role = row.role))
      setkeyv(self$row_info, "id")

      invisible(self)
    }

  ),

  active = list(
    feature_names = function() {
      self$col_info[role == "feature", "id"][[1L]]
    },

    target_names = function() {
      character(0L)
    },

    formula = function() {
      reformulate(self$feature_names)
    },

    nrow = function() {
      self$row_info[role == "training", .N]
    },

    ncol = function() {
      self$col_info[role %in% c("feature", "target"), .N]
    },

    col_types = function() {
      self$col_info[role %in% c("feature", "target"), c("id", "type")]
    }
  ),

  private = list(
    deep_clone = function(name, value) {
      if (name %in% c("row_info", "col_info")) copy(value) else value
    }
  )
)

assert_task = function(task) {
  assert_r6(task, "Task")
}

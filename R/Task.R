#' @title Basic Tasks
#' @format \code{\link{R6Class}} object
#'
#' @description
#' This is the abstract base class for task objects.
#' Use \code{\link{TaskClassif}} or \code{\link{TaskRegr}} to construct tasks instead of this class.
#'
#' @return [\code{\link{Task}}].
#' @export
#' @family Tasks
#' @include capabilities.R
#' @examples
#' task = Task$new("iris", data = iris)
#' task$formula
Task = R6Class("Task",
  # Base Class for Tasks
  public = list(
    task.type = NA_character_,
    id = NULL,
    backend = NULL,
    rows = NULL,
    cols = NULL,
    order = character(0L),
    blocking = character(0L),

    initialize = function(id, data) {
      self$id = assertString(id, min.chars = 1L)
      if (inherits(data, "Backend")) {
        self$backend = list(data)
      } else {
        assertDataFrame(data)
        self$backend = list(BackendDataTable$new(data = data))
      }

      cn = self$backend[[1L]]$colnames
      types = assertSubset(vcapply(self$backend[[1L]]$head(1L), class), capabilities$task.col.types, fmatch = TRUE)

      self$rows = data.table(
        id = self$backend[[1L]]$rownames,
        role = "training",
        key = "id")

      self$cols = data.table(
        id = cn,
        role = ifelse(cn == self$backend[[1L]]$primary_key, "primary_key", "feature"),
        type = types[chmatch(cn, names(types), 0L)],
        key = "id"
      )
    },

    print = function(...) {
      catf("Task '%s' of type %s (%i x %i)", self$id, self$task.type, self$nrow, self$ncol)
      catf(stri_list("Target: ", self$target))
      catf(stri_list("Features: ", self$features))
      catf(stri_list("Order by: ", self$order))
      catf(stri_list("Blocking: ", self$blocking))
      catf(stri_list("Public: ", setdiff(ls(self), c("initialize", "print"))))
    },

    data = function(rows = NULL, cols = NULL, filter.rows = TRUE, filter.cols = TRUE) {
      if (is.null(rows)) {
        selected.rows = if (filter.rows)
          self$rows[role == "training", "id"][[1L]]
        else
          self$rows$id
      } else {
        selected.rows = if (filter.rows)
          self$rows[id %in% rows & role == "training", "id"][[1L]]
        else
          self$rows[id %in% rows, "id"][[1L]]

        if (length(selected.rows) != length(rows))
          stopf("Invalid row ids provided")
      }

      if (is.null(cols)) {
        selected.cols = if (filter.cols)
          self$cols[role %in% c("feature", "target"), "id"][[1L]]
        else
          self$cols$id
      } else {
        selected.cols = if (filter.cols)
          self$cols[id %in% cols & role %in% c("feature", "target"), "id"][[1L]]
        else
          self$cols[id %in% cols, "id"][[1L]]

        if (length(selected.cols) != length(cols))
          stopf("Invalid col ids provided")
      }

      extra.cols = character(0L)
      if (length(self$order)) {
        extra.cols = setdiff(self$order, selected.cols)
        selected.cols = union(selected.cols, extra.cols)
      }

      data = rbindlist(
        lapply(self$backend, function(b) b$data(rows = selected.rows, cols = selected.cols)),
        fill = TRUE)

      if (nrow(data) != length(selected.rows)) {
        stopf("Backend did not return the rows correctly: %i requested, %i received", length(selected.rows), nrow(data))
      }

      if (ncol(data) != length(selected.cols)) {
        stopf("Backend did not return the cols correctly: %i requested, %i received", length(selected.cols), ncol(data))
      }

      if (length(self$order)) {
        setorderv(data, self$order)
      }

      if (length(extra.cols))
        data[, (extra.cols) := NULL]
      return(data)
    },

    head = function(n = 6L) {
      assertCount(n)
      ids = head(self$rows[role == "training", "id", with = FALSE][[1L]], n)
      self$data(rows = ids, cols = c(self$features, self$target))
    },

    row.ids = function(subset = NULL, as.vector = TRUE) {
      if (is.null(subset)) {
        result = self$rows[role == "training", "id"]
      } else {
        type = attr(subset, "subset.type")
        if (is.null(type)) {
          result = self$rows[role == "training"][as.integer(subset), "id"]
        } else {
          result = switch(type,
            "ids" = self$rows[.(subset), nomatch = 0L],
            "numbers" = self$rows[role == "training"][subset, "id"],
            "roles" = self$rows[role %in% subset, "id"],
            stopf("Unknown subset.type"))
        }
      }
      attr(result, "subset.type") = "ids"
      if (as.vector) result[[1L]] else result
    },

    add_backend = function(backend, row.role = "validation") {
      b = self$backend[[1L]]
      assertNames(backend$colnames, subset.of = b$colnames, must.include = b$primary_key)

      rn = backend$rownames
      for (b in self$backend) {
        clashes = b$data(rn, b$primary_key)
        if (nrow(clashes))
          stopf("Cannot add new backend: name clashes with primary_key id: %s", stri_peek(clashes[[1L]]))
      }

      self$backend[[length(self$backend) + 1L]] = backend
      self$rows = rbind(self$rows, data.table(id = rn, role = row.role))
      setkeyv(self$rows, "id")

      invisible(self)
    }

  ),

  active = list(
    features = function() {
      self$cols[role == "feature", "id"][[1L]]
    },

    target = function() {
      character(0L)
    },

    formula = function() {
      reformulate(self$features)
    },

    nrow = function() {
      self$rows[role == "training", .N]
    },

    ncol = function() {
      self$cols[role %in% c("feature", "target"), .N]
    },

    col.types = function() {
      self$cols[role %in% c("feature", "target"), c("id", "type")]
    }
  ),

  private = list(
    deep_clone = function(name, value) {
      if (name %in% c("rows", "cols")) copy(value) else value
    }
  )
)

assertTask = function(task) {
  assertR6(task, "Task")
}


    data = function(rows = NULL, cols = NULL, filter_rows = TRUE, filter_cols = TRUE) {
      if (is.null(rows)) {
        selected_rows = if (filter_rows)
          self$row_info[role == "use", "id"][[1L]]
        else
          self$row_info$id
      } else {
        selected_rows = if (filter_rows)
          self$row_info[id %in% rows & role == "use", "id"][[1L]]
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

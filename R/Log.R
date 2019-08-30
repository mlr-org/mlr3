Log = R6Class("Log",
  public = list(
    data = NULL,
    initialize = function() {
      self$data = data.table(
        stage = factor(levels = c("train", "predict")),
        class = factor(levels = c("output", "warning", "error"), ordered = TRUE),
        msg = character()
      )
    },

    append = function(stage, tab) {
      if (nrow(tab) > 0L) {
        tab$stage = stage
        self$data = rbind(self$data, tab)
      }
      invisible(self)
    }
  )
)

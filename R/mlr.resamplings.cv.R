#' @include Resampling.R
ResamplingCV = R6Class("ResamplingCV", inherit = Resampling,
  public = list(
    id = "cv",
    folds = 10L,
    instantiate = function(task, ...) {
      # inner function so we can easily implement blocking here
      # -> replace ids with unique values of blocking variable
      # -> join ids using blocks
      cv = function(ids, folds) {
        data.table(
          row.id = ids,
          fold = shuffle(seq_along0(ids) %% folds + 1L),
          key = "fold"
        )
      }
      assertTask(task)
      private$instance = cv(task$row.ids(), asInt(self$folds, lower = 1L))
      self
    },

    train.set = function(i) {
      i = assertResamplingIndex(self, i)
      private$instance[!.(i), "row.id", on = "fold"][[1L]]
    },

    test.set = function(i) {
      i = assertResamplingIndex(self, i)
      private$instance[.(i), "row.id", on = "fold"][[1L]]
    }
  ),

  active = list(
    iters = function() {
      self$folds
    }
  )
)

mlr.resamplings$add(
  ResamplingCV$new()
)

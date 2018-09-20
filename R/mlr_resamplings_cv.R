#' @include Resampling.R
ResamplingCV = R6Class("ResamplingCV", inherit = Resampling,
  public = list(
    initialize = function(id = "cv") {
      super$initialize(
        id = id,
        par_set = ParamSet$new(params = list(ParamInt$new("folds", lower = 1L))),
        par_vals = list(folds = 10L)
      )
      self$has_duplicates = FALSE
    },

    instantiate = function(task, ...) {
      # inner function so we can easily implement blocking here
      # -> replace ids with unique values of blocking variable
      # -> join ids using blocks
      cv = function(ids, folds) {
        data.table(
          row_id = ids,
          fold = shuffle(seq_along0(ids) %% folds + 1L),
          key = "fold"
        )
      }
      assert_task(task)
      private$.instantiate(cv(task$row_ids(), self$par_vals$folds))
    },

    train_set = function(i) {
      i = assert_resampling_index(self, i)
      private$.instance[!list(i), "row_id", on = "fold"][[1L]]
    },

    test_set = function(i) {
      i = assert_resampling_index(self, i)
      private$.instance[list(i), "row_id", on = "fold"][[1L]]
    }
  ),

  active = list(
    iters = function() {
      self$par_vals$folds
    }
  )
)


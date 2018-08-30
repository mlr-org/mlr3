#' @include Resampling.R
#' @include mlr_resamplings_subsampling.R
ResamplingHoldout = R6Class("ResamplingHoldout", inherit = Resampling,
  public = list(
    id = "holdout",
    ratio = 0.67,
    iters = 1L,
    instantiate = function(task, ...) {
      # inner function so we can easily implement blocking here
      # -> replace ids with unique values of blocking variable
      # -> join ids using blocks
      holdout = function(ids, ratio) {
        ii = sample(ids, ratio * length(ids))
        list(train = ii, test = setdiff(ids, ii))
      }

      assert_task(task)
      private$instance = holdout(task$row_ids(), assert_number(self$ratio, lower = 0))
      self
    },

    train_set = function(i) {
      i = assert_resampling_index(self, i)
      private$instance$train
    },

    test_set = function(i) {
      i = assert_resampling_index(self, i)
      private$instance$test
    }
  )
)

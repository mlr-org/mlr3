#' @include Resampling.R
#' @include mlr_resamplings_subsampling.R
ResamplingHoldout = R6Class("ResamplingHoldout", inherit = Resampling,
  public = list(
    initialize = function(id = "holdout") {
      super$initialize(
        id = id,
        par_set = ParamSet$new(params = list(ParamReal$new("ratio", lower = 0, upper = 1))),
        par_vals = list(ratio = 2/3)
      )
      self$has_duplicates = FALSE
    },

    instantiate = function(task, ...) {
      # inner function so we can easily implement blocking here
      # -> replace ids with unique values of blocking variable
      # -> join ids using blocks
      holdout = function(ids, ratio) {
        ii = shuffle(ids, round(ratio * length(ids)))
        list(train = ii, test = setdiff(ids, ii))
      }

      assert_task(task)
      private$.instance = holdout(task$row_ids(), assert_number(self$par_vals$ratio, lower = 0))
      private$.hash = NA_character_
      self
    },

    train_set = function(i) {
      i = assert_resampling_index(self, i)
      private$.instance$train
    },

    test_set = function(i) {
      i = assert_resampling_index(self, i)
      private$.instance$test
    },

    iters = 1L
  )
)

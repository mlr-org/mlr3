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
      assert_task(task)
      private$.hash = NA_character_
      self$instance = resampling_holdout(task$row_ids(), assert_number(self$par_vals$ratio, lower = 0))
      self
    },

    train_set = function(i) {
      i = assert_resampling_index(self, i)
      self$instance$train
    },

    test_set = function(i) {
      i = assert_resampling_index(self, i)
      self$instance$test
    },

    iters = 1L
  )
)

resampling_holdout = function(ids, ratio) {
  ii = shuffle(ids, round(ratio * length(ids)))
  list(train = ii, test = setdiff(ids, ii))
}

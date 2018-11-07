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
      self$instance = instantiate_holdout(task, self$par_vals$ratio, stratify = self$stratify)
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


#' @include mlr_resamplings.R
mlr_resamplings$add("holdout", ResamplingHoldout)


resample_holdout = function(ids, ratio) {
  nr = rround(length(ids) * ratio)
  ii = shuffle(ids, nr)
  list(train = ii, test = setdiff(ids, ii))
}


instantiate_holdout = function(task, ratio, stratify = character(0L)) {
  if (length(stratify) == 0L) {
    res = resample_holdout(task$row_ids(), ratio)
  } else {
    grps = stratify_groups(task, stratify = stratify)
    res = lapply(grps$..row_id, resample_holdout, ratio = ratio)
    res = Reduce(function(lhs, rhs) { list(train = c(lhs$train, rhs$train), test = c(lhs$test , rhs$test)) }, res)
  }
}

#' @include Resampling.R
#' @include mlr_resamplings_subsampling.R
ResamplingHoldout = R6Class("ResamplingHoldout", inherit = Resampling,
  public = list(
    initialize = function(id = "holdout") {
      super$initialize(
        id = id,
        param_set = ParamSet$new(params = list(ParamReal$new("ratio", lower = 0, upper = 1))),
        param_vals = list(ratio = 2/3)
      )
      self$has_duplicates = FALSE
    },

    instantiate = function(task, ...) {
      assert_task(task)
      private$.instantiate(instantiate_holdout(task, self$param_vals$ratio, stratify = self$stratify))
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
    res = resample_holdout(task$row_ids[[1L]], ratio)
  } else {
    grps = stratify_groups(task, stratify = stratify)
    res = lapply(grps$..row_id, resample_holdout, ratio = ratio)
    res = list(train = do.call(c, map(res, "train")), test = do.call(c, map(res, "test")))
  }
  res
}

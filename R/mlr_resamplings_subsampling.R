#' @include Resampling.R
ResamplingSubsampling = R6Class("ResamplingSubsampling", inherit = Resampling,
  public = list(
    initialize = function(id = "subsampling") {
      super$initialize(
        id = id,
        par_set = ParamSet$new(params = list(ParamInt$new("repeats", lower = 1), ParamReal$new("ratio", lower = 0, upper = 1))),
        par_vals = list(repeats = 30L, ratio = 0.67)
      )
      self$has_duplicates = FALSE
    },

    instantiate = function(task, ...) {
      assert_task(task)
      private$.instantiate(instantiate_subsampling(task, self$par_vals$ratio, self$par_vals$repeats, stratify = self$stratify))
    },

    train_set = function(i) {
      i = assert_resampling_index(self, i)
      self$instance$row_ids[bit::as.which(self$instance$train[[i]])]
    },

    test_set = function(i) {
      i = assert_resampling_index(self, i)
      self$instance$row_ids[bit::as.which(!self$instance$train[[i]])]
    }
  ),

  active = list(
    iters = function() {
      self$par_vals$repeats
    }
  )
)


#' @include mlr_resamplings.R
mlr_resamplings$add("subsampling", ResamplingSubsampling)



resample_subsampling = function(ids, ratio, repeats) {
  n = length(ids)
  nr = rround(n * ratio)

  train = replicate(repeats,
    bit::as.bit(replace(logical(n), sample.int(n, nr), TRUE)),
    simplify = FALSE)
  list(train = train, row_ids = ids)
}

instantiate_subsampling = function(task, ratio, repeats, stratify = character(0L)) {
  if (length(stratify) == 0L) {
    res = resample_subsampling(task$row_ids(), ratio, repeats)
  } else {
    grps = stratify_groups(task, stratify = stratify)
    res = lapply(grps$..row_id, resample_subsampling, ratio = ratio, repeats = repeats)
    res = Reduce(function(lhs, rhs) { list(train = Map(c, lhs$train, rhs$train), row_ids = c(lhs$row_ids , rhs$row_ids)) }, res)
  }
  res
}

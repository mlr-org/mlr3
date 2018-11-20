#' @include Resampling.R
ResamplingBootstrap = R6Class("ResamplingBootstrap", inherit = Resampling,
  public = list(
    initialize = function(id = "bootstrap") {
      super$initialize(
        id = id,
        param_set = ParamSet$new(params = list(
            ParamInt$new("repeats", lower = 1L),
            ParamReal$new("ratio", lower = 0, upper = 1))
        ),
        param_vals = list(ratio = 1, repeats = 30L)
      )
      self$has_duplicates = TRUE
    },

    instantiate = function(task, ...) {
      assert_task(task)
      private$.instantiate(instantiate_bootstrap(task, self$param_vals$ratio, self$param_vals$repeats, self$stratify))
    },

    train_set = function(i) {
      i = assert_resampling_index(self, i)
      rep(self$instance$row_ids, times = self$instance$M[, i])
    },

    test_set = function(i) {
      i = assert_resampling_index(self, i)
      self$instance$row_ids[self$instance$M[, i] == 0L]
    }
  ),

  active = list(
    iters = function() {
      self$param_vals$repeats
    }
  )
)


#' @include mlr_resamplings.R
mlr_resamplings$add("bootstrap", ResamplingBootstrap)


resample_bootstrap = function(ids, ratio, repeats) {
  nr = rround(length(ids) * ratio)
  x = factor(seq_along(ids))
  M = replicate(repeats, table(sample(x, nr, replace = TRUE)), simplify = "array")
  rownames(M) = NULL
  list(row_ids = ids, M = M)
}

instantiate_bootstrap = function(task, ratio, repeats, stratify = character(0L)) {
  if (length(stratify) == 0L) {
    res = resample_bootstrap(task$row_ids[[1L]], ratio, repeats)
  } else {
    grps = stratify_groups(task, stratify = stratify)
    res = lapply(grps$..row_id, resample_bootstrap, ratio = ratio, repeats = repeats)
    res = list(row_ids = do.call(c, pluck(res, "row_ids")), M = do.call(rbind, pluck(res, "M")))
  }

  res
}

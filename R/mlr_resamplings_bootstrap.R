#' @include Resampling.R
ResamplingBootstrap = R6Class("ResamplingBootstrap", inherit = Resampling,
  public = list(
    stratify = character(0L),
    initialize = function(id = "bootstrap") {
      super$initialize(
        id = id,
        par_set = ParamSet$new(params = list(
            ParamInt$new("repeats", lower = 1L),
            ParamReal$new("ratio", lower = 0, upper = 1))
        ),
        par_vals = list(ratio = 1, repeats = 30L)
      )
      self$has_duplicates = TRUE
    },

    instantiate = function(task, ...) {
      assert_task(task)
      private$.hash = NA_character_
      self$instance = instantiate_bootstrap(task, self$par_vals$ratio, self$par_vals$repeats, self$stratify)
      self
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
      self$par_vals$repeats
    }
  )
)


#' @include mlr_resamplings.R
mlr_resamplings$add("bootstrap", ResamplingBootstrap)


resample_bootstrap = function(ids, ratio, repeats) {
  nr = max(round(length(ids) * ratio), 1L)
  x = factor(seq_along(ids))
  M = replicate(repeats, table(sample(x, nr, replace = TRUE)), simplify = "array")
  rownames(M) = NULL
  list(row_ids = ids, M = M)
}

instantiate_bootstrap = function(task, ratio, repeats, stratify = character(0L)) {
  if (length(stratify) == 0L) {
    res = resample_bootstrap(task$row_ids(), ratio, repeats)
  } else {
    grps = stratify_groups(task, stratify = stratify, min_group_size = 2L)
    res = lapply(grps$..row_id, resample_bootstrap, ratio = ratio, repeats = repeats)
    res2 = Reduce(function(lhs, rhs) list(row_ids = c(lhs$row_ids, rhs$row_ids), M = rbind(lhs$M, rhs$M)), res)
  }
}

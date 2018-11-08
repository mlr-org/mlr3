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
      assert_task(task)
      private$.instantiate(instantiate_cv(task, self$par_vals$folds, self$stratify))
    },

    train_set = function(i) {
      i = assert_resampling_index(self, i)
      self$instance[!list(i), "row_id", on = "fold"][[1L]]
    },

    test_set = function(i) {
      i = assert_resampling_index(self, i)
      self$instance[list(i), "row_id", on = "fold"][[1L]]
    }
  ),

  active = list(
    iters = function() {
      self$par_vals$folds
    }
  )
)


#' @include mlr_resamplings.R
mlr_resamplings$add("cv", ResamplingCV)


resample_cv = function(ids, folds) {
  data.table(
    row_id = ids,
    fold = shuffle(seq_along0(ids) %% folds + 1L)
  )
}


instantiate_cv = function(task, folds, stratify = character(0L)) {
  if (length(stratify) == 0L) {
    res = resample_cv(task$row_ids(), folds)
  } else {
    grps = stratify_groups(task, stratify = stratify, min_group_size = folds)
    res = map_dtr(grps$..row_id, resample_cv, folds = folds)
  }

  setkeyv(res, "fold")[]
}

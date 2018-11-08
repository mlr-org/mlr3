#' @include Resampling.R
ResamplingRepeatedCV = R6Class("ResamplingRepeatedCV", inherit = Resampling,
  public = list(
    initialize = function(id = "repeated_cv") {
      super$initialize(
        id = id,
        par_set = ParamSet$new(params = list(ParamInt$new("repeats", lower = 1), ParamInt$new("folds", lower = 1L))),
        par_vals = list(repeats = 10L, folds = 10L)
      )
      self$has_duplicates = FALSE
    },

    instantiate = function(task, ...) {
      assert_task(task)
      private$.hash = NA_character_
      self$instance = instantiate_repeated_cv(task, self$par_vals$folds, self$par_vals$repeats, stratify = self$stratify)
      self
    },

    train_set = function(i) {
      i = assert_resampling_index(self, i) - 1L
      folds = as.integer(self$par_vals$folds)
      rep = as.integer(i %/% folds) + 1L
      fold = as.integer(i %% folds) + 1L
      ii = data.table(rep = rep, fold = setdiff(seq_len(folds), fold))
      self$instance[ii, "row_id"][[1L]]
    },

    test_set = function(i) {
      i = assert_resampling_index(self, i) - 1L
      folds = as.integer(self$par_vals$folds)
      rep = as.integer(i %/% folds) + 1L
      fold = as.integer(i %% folds) + 1L
      ii = data.table(rep = rep, fold = fold)
      self$instance[ii, "row_id"][[1L]]
    }
  ),

  active = list(
    iters = function() {
      self$par_vals$repeats * self$par_vals$folds
    }
  )
)


#' @include mlr_resamplings.R
mlr_resamplings$add("repeated_cv", ResamplingRepeatedCV)


resample_repeated_cv = function(ids, folds, repeats) {
  n = length(ids)
  map_dtr(seq_len(repeats), function(i) {
    data.table(row_id = ids, rep = i, fold = shuffle(seq_len0(n) %% folds + 1L))
  })
}


instantiate_repeated_cv = function(task, folds, repeats, stratify = character(0L)) {
  if (length(stratify) == 0L) {
    res = resample_repeated_cv(task$row_ids(), folds, repeats)
  } else {
    grps = stratify_groups(task, stratify = stratify, min_group_size = folds)
    res = map_dtr(grps$..row_id, resample_repeated_cv, folds = folds, repeats = repeats)
  }

  setkeyv(res, c("rep", "fold"))[]
}

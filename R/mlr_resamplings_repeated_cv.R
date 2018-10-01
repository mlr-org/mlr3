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
      self$instance = resampling_repeated_cv(task$row_ids(), self$par_vals$folds, self$par_vals$repeats)
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

resampling_repeated_cv = function(ids, folds, repeats) {
  n = length(ids)
  data = rbindlist(lapply(seq_len(repeats), function(i) {
    data.table(
      row_id = ids,
      rep = i,
      fold = shuffle(seq_len0(n) %% folds + 1L)
    )
  }))

  setkeyv(data, c("rep", "fold"))
}

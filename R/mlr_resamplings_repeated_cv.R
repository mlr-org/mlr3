#' @title Repeated Cross Validation Resampling
#'
#' @format [R6Class] object
#' @name mlr_resamplings_repeated_cv
#' @format [R6::R6Class()] inheriting from [Resampling].
#'
#' @description
#' `repeats` times repeated `folds`-fold cross validation.
#'
#' @include Resampling.R
#' @export
#' @examples
#' # Create a task with 10 observations
#' task = mlr_tasks$get("iris")
#' task$filter(1:10)
#'
#' # Instantiate Resampling
#' rrcv = mlr_resamplings$get("repeated_cv")
#' rrcv$param_vals = list(repeats = 2, folds = 3)
#' rrcv$instantiate(task)
#' rrcv$iters
#'
#' # Individual sets:
#' rrcv$train_set(1)
#' rrcv$test_set(1)
#' intersect(rrcv$train_set(1), rrcv$test_set(1))
#'
#' # Internal storage:
#' rrcv$instance # table
ResamplingRepeatedCV = R6Class("ResamplingRepeatedCV", inherit = Resampling,
  public = list(
    initialize = function(id = "repeated_cv") {
      super$initialize(
        id = id,
        param_set = ParamSet$new(params = list(ParamInt$new("repeats", lower = 1), ParamInt$new("folds", lower = 1L, tags = "required"))),
        param_vals = list(repeats = 10L, folds = 10L)
      )
      self$has_duplicates = FALSE
    },

    instantiate = function(task, ...) {
      assert_task(task)
      private$.instantiate(instantiate_repeated_cv(task, self$param_vals$folds, self$param_vals$repeats, stratify = self$stratify))
    },

    train_set = function(i) {
      i = assert_resampling_index(self, i) - 1L
      folds = as.integer(self$param_vals$folds)
      rep = as.integer(i %/% folds) + 1L
      fold = as.integer(i %% folds) + 1L
      ii = data.table(rep = rep, fold = setdiff(seq_len(folds), fold))
      self$instance[ii, "row_id"][[1L]]
    },

    test_set = function(i) {
      i = assert_resampling_index(self, i) - 1L
      folds = as.integer(self$param_vals$folds)
      rep = as.integer(i %/% folds) + 1L
      fold = as.integer(i %% folds) + 1L
      ii = data.table(rep = rep, fold = fold)
      self$instance[ii, "row_id"][[1L]]
    }
  ),

  active = list(
    iters = function() {
      self$param_vals$repeats * self$param_vals$folds
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
    res = resample_repeated_cv(task$row_ids[[1L]], folds, repeats)
  } else {
    grps = stratify_groups(task, stratify = stratify, min_group_size = folds)
    res = map_dtr(grps$..row_id, resample_repeated_cv, folds = folds, repeats = repeats)
  }

  setkeyv(res, c("rep", "fold"))[]
}

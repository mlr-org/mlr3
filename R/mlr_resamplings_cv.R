#' @title Cross Validation Resampling
#'
#' @format [R6Class] object
#' @name mlr_resamplings_cv
#' @format [R6::R6Class] inheriting from [Resampling].
#'
#' @description
#' Cross validation with `folds` folds (default: 10).
#'
#' @include Resampling.R
#' @export
#' @examples
#' # Create a task with 10 observations
#' task = mlr_tasks$get("iris")
#' task$filter(1:10)
#'
#' # Instantiate Resampling
#' rcv = mlr_resamplings$get("cv")
#' rcv$param_vals = list(folds = 3)
#' rcv$instantiate(task)
#'
#' # Individual sets:
#' rcv$train_set(1)
#' rcv$test_set(1)
#' intersect(rcv$train_set(1), rcv$test_set(1))
#'
#' # Internal storage:
#' rcv$instance # table
ResamplingCV = R6Class("ResamplingCV", inherit = Resampling,
  public = list(
    initialize = function(id = "cv") {
      super$initialize(
        id = id,
        param_set = ParamSet$new(params = list(ParamInt$new("folds", lower = 1L, tags = "required"))),
        param_vals = list(folds = 10L)
      )
      self$has_duplicates = FALSE
    },

    instantiate = function(task, ...) {
      assert_task(task)
      private$.instantiate(instantiate_cv(task, self$param_vals$folds, self$stratify))
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
      self$param_vals$folds
    }
  )
)


#' @include mlr_resamplings.R
mlr_resamplings$add("cv", ResamplingCV)


resample_cv = function(ids, folds) {
  data.table(
    row_id = ids,
    fold = shuffle(seq_along0(ids) %% folds + 1L),
    key = "fold"
  )
}


instantiate_cv = function(task, folds, stratify = character(0L)) {
  if (length(stratify) == 0L) {
    res = resample_cv(task$row_ids[[1L]], folds)
  } else {
    grps = stratify_groups(task, stratify = stratify, min_group_size = folds)
    res = map_dtr(grps$..row_id, resample_cv, folds = folds)
  }

  setkeyv(res, "fold")[]
}

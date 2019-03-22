#' @title Repeated Cross Validation Resampling
#'
#' @name mlr_resamplings_repeated_cv
#' @format [R6::R6Class()] inheriting from [Resampling].
#' @include Resampling.R
#'
#' @description
#' `repeats` (default: 10) times repeated `folds`-fold (default: 10) cross-validation.
#'
#' The iteration counter translates to `repeats` blocks of `folds`
#' cross-validations, i.e., the first `folds` iterations belong to
#' a single cross-validation.
#'
#' @section Fields:
#' @inheritSection Learner Fields
#'
#' @section Methods:
#' * `folds(iters)`\cr
#'   `integer()` -> `integer()`\cr
#'   Translates iteration numbers to fold number.
#'
#' * `repeats(iters)`\cr
#'   `integer()` -> `integer()`\cr
#'   Translates iteration numbers to repetition number.
#'
#' @inheritSection Learner Methods
#'
#' @export
#' @examples
#' # Create a task with 10 observations
#' task = mlr_tasks$get("iris")
#' task$filter(1:10)
#'
#' # Instantiate Resampling
#' rrcv = mlr_resamplings$get("repeated_cv")
#' rrcv$param_set$values = list(repeats = 2, folds = 3)
#' rrcv$instantiate(task)
#' rrcv$iters
#' rrcv$folds(1:6)
#' rrcv$repeats(1:6)
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
    initialize = function(id = "repeated_cv", param_vals = list(repeats = 10L, folds = 10L)) {
      super$initialize(
        id = id,
        param_set = ParamSet$new(params = list(ParamInt$new("repeats", lower = 1), ParamInt$new("folds", lower = 1L, tags = "required"))),
        param_vals = param_vals
      )
    },

    folds = function(iters) {
      iters = assert_integerish(iters, any.missing = FALSE, coerce = TRUE)
      ((iters - 1L) %% self$param_set$values$repeats) + 1L
    },

    repeats = function(iters) {
      iters = assert_integerish(iters, any.missing = FALSE, coerce = TRUE)
      ((iters - 1L) %/% self$param_set$values$folds) + 1L
    }
  ),

  active = list(
    iters = function() {
      pv = self$param_set$values
      pv$repeats * pv$folds
    }
  ),

  private = list(
    .sample = function(ids) {
      pv = self$param_set$values
      n = length(ids)
      folds = pv$folds
      map_dtr(seq_len(pv$repeats), function(i) {
        data.table(row_id = ids, rep = i, fold = shuffle(seq_len0(n) %% folds + 1L))
      })
    },

    .get_train = function(i) {
      i = i - 1L
      folds = as.integer(self$param_set$values$folds)
      rep = as.integer(i %/% folds) + 1L
      fold = as.integer(i %% folds) + 1L
      ii = data.table(rep = rep, fold = setdiff(seq_len(folds), fold))
      self$instance[ii, "row_id", on = names(ii), nomatch = 0L][[1L]]
    },

    .get_test = function(i) {
      i = i - 1L
      folds = as.integer(self$param_set$values$folds)
      rep = as.integer(i %/% folds) + 1L
      fold = as.integer(i %% folds) + 1L
      ii = data.table(rep = rep, fold = fold)
      self$instance[ii, "row_id", on = names(ii), nomatch = 0L][[1L]]
    },

    .combine = function(instances) {
      rbindlist(instances)
    }
  )
)


#' @include mlr_resamplings.R
mlr_resamplings$add("repeated_cv", ResamplingRepeatedCV)

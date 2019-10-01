#' @title Repeated Cross Validation Resampling
#'
#' @usage NULL
#' @aliases mlr_resamplings_repeated_cv
#' @format [R6::R6Class()] inheriting from [Resampling].
#' @include Resampling.R
#'
#' @section Construction:
#' ```
#' ResamplingRepeatedCV$new()
#' mlr_resamplings$get("repeated_cv")
#' rsmp("repeated_cv")
#' ```
#'
#' @description
#' Splits data `repeats` (default: 10) times using a `folds`-fold (default: 10) cross-validation.
#'
#' The iteration counter translates to `repeats` blocks of `folds`
#' cross-validations, i.e., the first `folds` iterations belong to
#' a single cross-validation.
#'
#' @section Fields:
#' See [Resampling].
#'
#' @section Methods:
#' See [Resampling].
#' Additionally, the class provides two helper function to translate iteration numbers to folds / repeats:
#'
#' * `folds(iters)`\cr
#'   `integer()` -> `integer()`\cr
#'   Translates iteration numbers to fold number.
#'
#' * `repeats(iters)`\cr
#'   `integer()` -> `integer()`\cr
#'   Translates iteration numbers to repetition number.
#'
#' @section Parameters:
#' * `stratify` :: `logical(1)` | `character()`\cr
#'   Enables stratification. See [Resampling].
#' * `repeats` :: `integer(1)`\cr
#'   Number of repetitions.
#' * `folds` :: `integer(1)`\cr
#'   Number of folds.
#'
#' @family Resampling
#' @template seealso_resampling
#' @export
#' @examples
#' # Create a task with 10 observations
#' task = tsk("iris")
#' task$filter(1:10)
#'
#' # Instantiate Resampling
#' rrcv = rsmp("repeated_cv", repeats = 2, folds = 3)
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
    initialize = function() {
      ps = ParamSet$new(list(
        ParamUty$new("stratify", default = NULL),
        ParamInt$new("repeats", lower = 1),
        ParamInt$new("folds", lower = 1L, tags = "required")
      ))
      ps$values = list(repeats = 10L, folds = 10L)
      super$initialize(id = "repeated_cv", param_set = ps)
    },

    folds = function(iters) {
      iters = assert_integerish(iters, any.missing = FALSE, coerce = TRUE)
      ((iters - 1L) %% as.integer(self$param_set$values$repeats)) + 1L
    },

    repeats = function(iters) {
      iters = assert_integerish(iters, any.missing = FALSE, coerce = TRUE)
      ((iters - 1L) %/% as.integer(self$param_set$values$folds)) + 1L
    }
  ),

  active = list(
    iters = function() {
      pv = self$param_set$values
      as.integer(pv$repeats) * as.integer(pv$folds)
    }
  ),

  private = list(
    .sample = function(ids) {
      pv = self$param_set$values
      n = length(ids)
      folds = as.integer(pv$folds)
      map_dtr(seq_len(pv$repeats), function(i) {
        data.table(row_id = ids, rep = i, fold = shuffle(seq_len0(n) %% folds + 1L))
      })
    },

    .get_train = function(i) {
      i = as.integer(i) - 1L
      folds = as.integer(self$param_set$values$folds)
      rep = i %/% folds + 1L
      fold = i %% folds + 1L
      ii = data.table(rep = rep, fold = seq_len(folds)[-fold])
      self$instance[ii, "row_id", on = names(ii), nomatch = 0L][[1L]]
    },

    .get_test = function(i) {
      i = as.integer(i) - 1L
      folds = as.integer(self$param_set$values$folds)
      rep = i %/% folds + 1L
      fold = i %% folds + 1L
      ii = data.table(rep = rep, fold = fold)
      self$instance[ii, "row_id", on = names(ii), nomatch = 0L][[1L]]
    },

    .combine = function(instances) {
      rbindlist(instances, use.names = TRUE)
    }
  )
)

#' @include mlr_resamplings.R
mlr_resamplings$add("repeated_cv", ResamplingRepeatedCV)

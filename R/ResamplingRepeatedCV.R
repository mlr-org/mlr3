#' @title Repeated Cross-Validation Resampling
#'
#' @name mlr_resamplings_repeated_cv
#' @include Resampling.R
#'
#' @description
#' Splits data `repeats` (default: 10) times using a `folds`-fold (default: 10) cross-validation.
#'
#' The iteration counter translates to `repeats` blocks of `folds`
#' cross-validations, i.e., the first `folds` iterations belong to
#' a single cross-validation.
#'
#' Iteration numbers can be translated into folds or repeats with provided methods.
#'
#' @templateVar id repeated_cv
#' @template section_dictionary_resampling
#'
#' @section Parameters:
#' * `repeats` (`integer(1)`)\cr
#'   Number of repetitions.
#' * `folds` (`integer(1)`)\cr
#'   Number of folds.
#'
#' @references
#' `r format_bib("bischl_2012")`
#'
#' @template seealso_resampling
#' @export
#' @examples
#' # Create a task with 10 observations
#' task = tsk("penguins")
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
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      ps = ps(
        folds   = p_int(2L, tags = "required")
        repeats = p_int(1L),
      )
      ps$values = list(repeats = 10L, folds = 10L)
      super$initialize(id = "repeated_cv", param_set = ps, man = "mlr3::mlr_resamplings_repeated_cv")
    },

    #' @description
    #' Translates iteration numbers to fold numbers.
    #' @param iters (`integer()`)\cr
    #'   Iteration number.
    #' @return `integer()` of fold numbers.
    folds = function(iters) {
      iters = assert_integerish(iters, any.missing = FALSE, coerce = TRUE)
      ((iters - 1L) %% as.integer(self$param_set$values$repeats)) + 1L
    },

    #' @description
    #' Translates iteration numbers to repetition numbers.
    #' @param iters (`integer()`)\cr
    #'   Iteration number.
    #' @return `integer()` of repetition numbers.
    repeats = function(iters) {
      iters = assert_integerish(iters, any.missing = FALSE, coerce = TRUE)
      ((iters - 1L) %/% as.integer(self$param_set$values$folds)) + 1L
    }
  ),

  active = list(
    #' @template field_iters
    iters = function(rhs) {
      assert_ro_binding(rhs)
      pv = self$param_set$values
      as.integer(pv$repeats) * as.integer(pv$folds)
    }
  ),

  private = list(
    .sample = function(ids, ...) {
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
      self$instance[ii, "row_id", on = names(ii), nomatch = NULL][[1L]]
    },

    .get_test = function(i) {
      i = as.integer(i) - 1L
      folds = as.integer(self$param_set$values$folds)
      rep = i %/% folds + 1L
      fold = i %% folds + 1L
      ii = data.table(rep = rep, fold = fold)
      self$instance[ii, "row_id", on = names(ii), nomatch = NULL][[1L]]
    },

    .combine = function(instances) {
      rbindlist(instances, use.names = TRUE)
    },

    deep_clone = function(name, value) {
      switch(name,
        "instance" = copy(value),
        "param_set" = value$clone(deep = TRUE),
        value
      )
    }
  )
)

#' @include mlr_resamplings.R
mlr_resamplings$add("repeated_cv", ResamplingRepeatedCV)

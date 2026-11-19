#' @title Cross-Validation Resampling
#'
#' @name mlr_resamplings_cv
#' @include Resampling.R
#'
#' @description
#' Splits data using a `folds`-folds (default: 10 folds) cross-validation.
#'
#' @templateVar id cv
#' @template resampling
#'
#' @section Parameters:
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
#' cv = rsmp("cv", folds = 3)
#' cv$instantiate(task)
#'
#' # Individual sets:
#' cv$train_set(1)
#' cv$test_set(1)
#'
#' # Disjunct sets:
#' intersect(cv$train_set(1), cv$test_set(1))
#'
#' # Internal storage:
#' cv$instance # table
ResamplingCV = R6Class(
  "ResamplingCV",
  inherit = Resampling,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      ps = ps(
        folds = p_int(2L, tags = "required")
      )
      ps$set_values(folds = 10L)

      super$initialize(id = "cv", param_set = ps, label = "Cross-Validation", man = "mlr3::mlr_resamplings_cv")
    }
  ),

  active = list(
    #' @template field_iters
    iters = function(rhs) {
      assert_ro_binding(rhs)
      as.integer(self$param_set$values$folds)
    }
  ),

  private = list(
    .sample = function(ids, task = NULL, ...) {
      assert_cv_folds(as.integer(self$param_set$values$folds), ids, task)
      data.table(
        row_id = ids,
        fold = shuffle(seq_along0(ids) %% as.integer(self$param_set$values$folds) + 1L),
        key = "fold"
      )
    },

    .get_train = function(i) {
      self$instance[!list(i), "row_id", on = "fold"][[1L]]
    },

    .get_test = function(i) {
      self$instance[list(i), "row_id", on = "fold", nomatch = NULL][[1L]]
    },

    .combine = function(instances) {
      cv_combine_strata(instances, as.integer(self$param_set$values$folds))
    },

    deep_clone = function(name, value) {
      # fmt: skip
      switch(name,
        "instance" = copy(value),
        ".param_set" = value$clone(deep = TRUE),
        value
      )
    }
  )
)

# Without strata, more folds than observations (or groups) leave some test sets empty.
# With strata, small strata are allowed as long as the other strata fill the folds.
assert_cv_folds = function(folds, ids, task) {
  if (folds > length(ids) && (is.null(task) || is.null(task$strata))) {
    error_input(
      "Cannot split %i %s into %i folds",
      length(ids),
      if (is.null(task) || is.null(task$groups)) "observations" else "groups",
      folds
    )
  }
}

# Each stratum assigns its remainder (`n %% folds` rows) to the first folds.
# Rotate the fold labels so that a stratum continues where the previous one stopped,
# otherwise the first folds grow with every stratum.
cv_combine_strata = function(instances, folds, repeats = 1L) {
  offset = 0L
  for (instance in instances) {
    set(instance, j = "fold", value = (instance$fold - 1L + offset) %% folds + 1L)
    offset = (offset + nrow(instance) %/% repeats) %% folds
  }
  rbindlist(instances, use.names = TRUE)
}

#' @include mlr_resamplings.R
mlr_resamplings$add("cv", function() ResamplingCV$new())

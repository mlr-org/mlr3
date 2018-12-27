#' @title Resampling Class
#'
#' @name Resampling
#' @format [R6Class] object.
#' @description
#' Abstraction for resampling strategies.
#' Predefined resamplings are stored in [mlr_resamplings].
#'
#' @section Usage:
#' ```
#' # Construction
#' r = Resampling$new(id, param_set, param_vals)
#'
#' # Members
#' r$has_duplicates
#' r$hash
#' r$id
#' r$instance
#' r$is_instantiated
#' r$iters
#' r$param_set
#' r$param_vals
#' r$stratify
#' r$task_hash
#'
#' # Methods
#' r$instantiate(task)
#' r$test_set(i)
#' r$train_set(i)
#' ```
#'
#' @section Arguments:
#' * `id` (`character(1)`): identifier for this object.
#' * `param_set` ([paradox::ParamSet]): describes available parameters.
#' * `param_vals` (`list()`): Named list of parameter values.
#' * `task` ([Task]): Object of type [Task].
#' * `i` (`integer(1)`): Get the `i`-th training/test set.
#'
#' @section Details:
#' * `$has_duplicates` is `TRUE` if the resampling allows observations to be included multiple times in the train set.
#'   E.g., this is true for bootstraping, but not for cross validation.
#' * `$hash` (`character(1)`) stores a checksum calculated on the `id`, `param_vals` and the instantiation.
#'   If the object is not instantiated yet, `NA` is returned.
#' * `$id` (`character(1)`) stores the identifier of the object.
#' * `$instance` stores the instantiated realization of the resampling.
#'    This is an arbitrary object, do not work directly with it. Instead, use `$train_set()` and `$test_set()`.
#' * `$instantiate` materializes fixed training and test splits for a given task.
#' * `$is_instantiated` returns `TRUE` if the resampling has been instantiated, and `FALSE` otherwise.
#' * `$iters` (`integer(1)`) calculates the resulting number of iterations, given the current `param_vals`.
#' * `$new()` creates a new object of class [Resampling].
#' * `$param_set` ([paradox::ParamSet]) describes available parameters.
#' * `$param_vals` (named `list`) stores the currently set parameter values.
#'    You can set parameters by assigning a named list of new parameters to this slot.
#' * `$stratify` can be set to column names of the [Task] which will be used for stratification during instantiation.
#' * `$task_hash` stores the hash of the task for which the resampling has been instantiated.
#' * `$test_set()` returns the test set for the `i`-th iteration.
#' * `$train_set()` returns the training set for the `i`-th iteration.
#'
#' @family Resampling
#' @examples
#' r = mlr_resamplings$get("subsampling")
#'
#' # Default parametrization
#' r$param_vals
#'
#' # Do only 3 repeats on 10% of the data
#' r$param_vals = list(ratio = 0.1, repeats = 3)
#' r$param_vals
#'
#' # Instantiate on iris task
#' task = mlr_tasks$get("iris")
#' r$instantiate(task)
#'
#' # Extract train/test sets
#' train_set = r$train_set(1)
#' print(train_set)
#' intersect(train_set, r$test_set(1))
#'
#' # Another example: 10-fold CV
#' r = mlr_resamplings$get("cv")$instantiate(task)
#' r$train_set(1)
#'
#' # Stratification
#' task = mlr_tasks$get("pima")
#' prop.table(table(task$truth())) # moderately unbalanced
#'
#' r = mlr_resamplings$get("subsampling")
#' r$stratify = task$target_names # stratify on target column
#' r$instantiate(task)
#' prop.table(table(task$truth(r$train_set(1)))) # roughly same proportion
#' prop.table(table(task$truth(r$train_set(1)))) # roughly same proportion # FIXME why two times?
NULL

#' @export
Resampling = R6Class("Resampling",
  public = list(
    id = NULL,
    param_set = NULL,
    stratify = NULL,
    instance = NULL,
    task_hash = NA_character_,
    has_duplicates = NA,

    initialize = function(id, param_set = ParamSet$new(), param_vals = list()) {
      self$id = assert_id(id)
      self$param_set = assert_param_set(param_set)
      private$.param_vals = assert_param_vals(param_vals, param_set)
      self$stratify = character(0L)
    },

    format = function() {
      sprintf("<%s>", class(self)[1L])
    },

    print = function(...) {
      pv = self$param_vals
      catf("%s with %i iterations", format(self), self$iters)
      catf(str_indent("Instantiated:", self$is_instantiated))
      catf(str_indent("Parameters:", as_short_string(pv, 1000L)))
      catf(str_indent("\nPublic:", setdiff(ls(self), c("initialize", "print"))))
    }
  ),

  active = list(
    hash = function() {
      if (is.null(self$instance))
        return(NA_character_)
      if (is.na(private$.hash))
        private$.hash = digest::digest(list(self$id, self$param_vals, self$instance), algo = "xxhash64")
      private$.hash
    },

    param_vals = function(rhs) {
      if (missing(rhs))
        return(private$.param_vals)
      private$.param_vals = assert_param_vals(rhs, self$param_set)
    },

    is_instantiated = function() {
      !is.null(self$instance)
    }
  ),

  private = list(
    .param_vals = NULL,
    .hash = NA_character_,
    .instantiate = function(task, instance) {
      self$instance = instance
      self$task_hash = task$hash
      private$.hash = NA_character_
      self
    }
  )
)

stratify_groups = function(task, stratify, min_group_size = 0L) {
  assert_subset(stratify, c(task$target_names, task$feature_names), empty.ok = FALSE)
  row_ids = task$row_ids[[1L]]
  grps = cbind(task$data(rows = row_ids, cols = stratify), ..row_id = row_ids)[, list(..N = .N, ..row_id = list(.SD$..row_id)), by = stratify]
  if (min_group_size > 0L) {
    ii = wf(grps$..N < min_group_size)
    if (length(ii)) {
      tmp = as.list(grps[ii, stratify, with = FALSE])
      stopf("Cannot stratify: combination %s has only %i observations",
        paste0(sprintf("[%s == '%s']", names(tmp), tmp), collapse = "x"),
        grps$..N[ii]
      )
    }
  }
  grps
}

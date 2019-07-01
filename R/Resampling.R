#' @title Resampling Class
#'
#' @usage NULL
#' @format [R6::R6Class] object.
#'
#' @description
#' This is the abstract base class for resampling objects like [ResamplingCV] and [ResamplingBootstrap].
#'
#' The objects of this class define how a task is partitioned for resampling (e.g., in [resample()] or [benchmark()]),
#' using a set of hyperparameters such as the number of folds in cross-validation.
#'
#' Resampling objects can be instantiated on a [Task], which applies the strategy on the task and manifests in a
#' fixed partition of `row_ids` of the [Task].
#'
#' Predefined resamplings are stored in the [Dictionary] [mlr_resamplings],
#' e.g. [`cv`][mlr_resamplings_cv] or [`bootstrap`][mlr_resamplings_bootstrap].
#'
#' @section Construction:
#' Note: This object is typically constructed via a derived classes, e.g. [ResamplingCV] or [ResamplingHoldout].
#' ```
#' r = Resampling$new(id, param_set, param_vals)
#' ```
#'
#' * `id` :: `character(1)`\cr
#'   Identifier for the resampling strategy.
#'
#' * `param_set` :: [paradox::ParamSet]\cr
#'   Set of hyperparameters.
#'
#' * `param_vals` :: named `list()`\cr
#'   List of hyperparameter settings.
#'
#' @section Fields:
#' * `id` :: `character(1)`\cr
#'   Identifier of the learner.
#'
#' * `param_set` :: [paradox::ParamSet]\cr
#'   Description of available hyperparameters and hyperparameter settings.
#'
#' * `hash` :: `character(1)`\cr
#'   Hash (unique identifier) for this object.
#'
#' * `instance` :: `any`\cr
#'   During `instantiate()`, the instance is stored in this slot.
#'   The instance can be in any arbitrary format.
#'
#' * `is_instantiated` :: `logical(1)`\cr
#'   Is `TRUE`, if the resampling has been instantiated.
#'
#' * `duplicated_ids` :: `logical(1)`\cr
#'   Is `TRUE` if this resampling strategy may have duplicated row ids in a single training set or test set.
#'   E.g., this is `TRUE` for Bootstrap, and `FALSE` for cross validation.
#'
#' * `iters` :: `integer(1)`\cr
#'   Return the number of resampling iterations, depending on the values stored in the `param_set`.
#'
#' * `task_hash` :: `character(1)`\cr
#'   The hash of the task which was passed to `r$instantiate()`.
#'
#' @section Methods:
#' * `instantiate(task)`\cr
#'   [Task] -> `self`\cr
#'   Materializes fixed training and test splits for a given task and stores them in `r$instance`.
#'
#' * `train_set(i)`\cr
#'   `integer(1)` -> (`integer()` | `character()`)\cr
#'   Returns the row ids of the i-th training set.
#'
#' * `test_set(i)`\cr
#'   `integer(1)` -> (`integer()` | `character()`)\cr
#'   Returns the row ids of the i-th test set.
#'
#' @export
#' @family Resampling
#' @examples
#' r = mlr_resamplings$get("subsampling")
#'
#' # Default parametrization
#' r$param_set$values
#'
#' # Do only 3 repeats on 10% of the data
#' r$param_set$values = list(ratio = 0.1, repeats = 3)
#' r$param_set$values
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
#' r$instantiate(task)
#' prop.table(table(task$truth(r$train_set(1)))) # roughly same proportion
Resampling = R6Class("Resampling",
  public = list(
    id = NULL,
    param_set = NULL,
    instance = NULL,
    task_hash = NA_character_,
    duplicated_ids = NULL,

    initialize = function(id, param_set = ParamSet$new(), param_vals = list(), duplicated_ids = FALSE) {
      self$id = assert_string(id, min.chars = 1L)
      self$param_set = assert_param_set(param_set)
      self$param_set$values = param_vals
      self$duplicated_ids = assert_flag(duplicated_ids)
    },

    format = function() {
      sprintf("<%s>", class(self)[1L])
    },

    print = function(...) {
      pv = self$param_set$values
      catf("%s with %i iterations", format(self), self$iters)
      catf(str_indent("Instantiated:", self$is_instantiated))
      catf(str_indent("Parameters:", as_short_string(pv, 1000L)))
      catf(str_indent("\nPublic:", setdiff(ls(self), c("initialize", "print"))))
    },

    instantiate = function(task) {
      assert_task(task)
      groups = task$groups

      stratify = self$param_set$values$stratify
      if (length(stratify) == 0L || isFALSE(stratify)) {
        if (is.null(groups)) {
          instance = private$.sample(task$row_ids)
        } else {
          private$.groups = groups
          instance = private$.sample(unique(groups$group))
        }
      } else {
        if (!is.null(groups)) {
          stopf("Cannot combine stratification with grouping")
        }
        instances = stratify(task, stratify)
        instance = private$.combine(lapply(instances$..row_id, private$.sample))
      }

      self$instance = instance
      self$task_hash = task$hash
      invisible(self)
    },

    train_set = function(i) {
      private$.get_set(private$.get_train, i)
    },

    test_set = function(i) {
      private$.get_set(private$.get_test, i)
    }
  ),

  active = list(
    is_instantiated = function() {
      !is.null(self$instance)
    },

    hash = function() {
      hash(list(class(self), self$id, self$param_set$values, self$instance))
    }
  ),

  private = list(
    .groups = NULL,

    .get_set = function(getter, i) {
      if (!self$is_instantiated) {
        stopf("Resampling '%s' has not been instantiated yet", self$id)
      }
      i = assert_int(i, lower = 1L, upper = self$iters, coerce = TRUE)
      ids = getter(i)

      if (is.null(private$.groups)) ids else private$.groups[ids, on = "group"][[1L]]
    }
  )
)

stratify = function(task, stratify) {
  if (isTRUE(stratify)) {
    stratify = task$target_names
  } else {
    assert_subset(stratify, c(task$target_names, task$feature_names), empty.ok = FALSE)
  }
  row_ids = task$row_ids
  cbind(task$data(rows = row_ids, cols = stratify), ..row_id = row_ids)[, list(..N = .N, ..row_id = list(.SD$..row_id)), by = stratify]
}

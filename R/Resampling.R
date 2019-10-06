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
#' Predefined resamplings are stored in the [mlr3misc::Dictionary] [mlr_resamplings],
#' e.g. [`cv`][mlr_resamplings_cv] or [`bootstrap`][mlr_resamplings_bootstrap].
#'
#' @section Construction:
#' Note: This object is typically constructed via a derived classes, e.g. [ResamplingCV] or [ResamplingHoldout].
#' ```
#' r = Resampling$new(id, param_set, duplicated_ids = FALSE, man = NA_character_)
#' ```
#'
#' * `id` :: `character(1)`\cr
#'   Identifier for the resampling strategy.
#'
#' * `param_set` :: [paradox::ParamSet]\cr
#'   Set of hyperparameters.
#'
#' * `duplicated_ids` :: `logical(1)`\cr
#'   Set to `TRUE` if this resampling strategy may have duplicated row ids in a single training set or test set.
#'
#' * `man` :: `character(1)`\cr
#'   String in the format `[pkg]::[topic]` pointing to a manual page for this object.
#'
#' @section Fields:
#' All variables passed to the constructor, and additionally:
#'
#' * `iters` :: `integer(1)`\cr
#'   Return the number of resampling iterations, depending on the values stored in the `param_set`.
#'
#' * `instance` :: `any`\cr
#'   During `instantiate()`, the instance is stored in this slot.
#'   The instance can be in any arbitrary format.
#'
#' * `is_instantiated` :: `logical(1)`\cr
#'   Is `TRUE`, if the resampling has been instantiated.
#'
#' * `task_hash` :: `character(1)`\cr
#'   The hash of the task which was passed to `r$instantiate()`.
#'
#' * `hash` :: `character(1)`\cr
#'   Hash (unique identifier) for this object.
#'
#'   E.g., this is `TRUE` for Bootstrap, and `FALSE` for cross validation.
#'   Only used internally.
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
#' * `help()`\cr
#'   () -> `NULL`\cr
#'   Opens the corresponding help page referenced by `$man`.
#'
#' @section Stratification:
#' All derived classes support stratified sampling.
#' The stratification variables are assumed to be discrete and must be stored in the [Task] with column role `"stratify"`.
#' In case of multiple stratification variables, each combination of the values of the stratification variables forms a strata.
#'
#' First, the observations are divided into subpopulations based one or multiple stratification variables (assumed to be discrete), c.f. `task$stratify`.
#'
#'
#' Second, the sampling is performed in each of the `k` subpopulations separately.
#' Each subgroup is divided into `iter` training sets and `iter` test sets by the derived `Resampling`.
#' These sets are merged based on their iteration number: all training sets from all subpopulations with iteration 1 are combined, then all training sets with iteration 2, and so on.
#' Same is done for all test sets.
#' The merged sets can be accessed via `$train_set(i)` and `$test_set(i)`, respectively.
#'
#'
#' @section Grouping / Blocking:
#' All derived classes support grouping of observations.
#' The grouping variable is assumed to be discrete and must be stored in the [Task] with column role `"groups"`.
#'
#' Observations in the same group are treated like a "block" of observations which must be kept together.
#' These observations either all go together into the training set or together into the test set.
#'
#' The sampling is performed by the derived [Resampling] on the grouping variable.
#' Next, the grouping information is replaced with the respective row ids to generate training and test sets.
#' The sets can be accessed via `$train_set(i)` and `$test_set(i)`, respectively.
#'
#' @family Resampling
#' @template seealso_resampling
#' @export
#' @examples
#' r = rsmp("subsampling")
#'
#' # Default parametrization
#' r$param_set$values
#'
#' # Do only 3 repeats on 10% of the data
#' r$param_set$values = list(ratio = 0.1, repeats = 3)
#' r$param_set$values
#'
#' # Instantiate on iris task
#' task = tsk("iris")
#' r$instantiate(task)
#'
#' # Extract train/test sets
#' train_set = r$train_set(1)
#' print(train_set)
#' intersect(train_set, r$test_set(1))
#'
#' # Another example: 10-fold CV
#' r = rsmp("cv")$instantiate(task)
#' r$train_set(1)
#'
#' # Stratification
#' task = tsk("pima")
#' prop.table(table(task$truth())) # moderately unbalanced
#' task$col_roles$stratify = task$target_names
#'
#' r = rsmp("subsampling")
#' r$instantiate(task)
#' prop.table(table(task$truth(r$train_set(1)))) # roughly same proportion
Resampling = R6Class("Resampling",
  public = list(
    id = NULL,
    param_set = NULL,
    instance = NULL,
    task_hash = NA_character_,
    duplicated_ids = NULL,
    man = NULL,

    initialize = function(id, param_set = ParamSet$new(), duplicated_ids = FALSE, man = NA_character_) {
      self$id = assert_string(id, min.chars = 1L)
      self$param_set = assert_param_set(param_set)
      self$duplicated_ids = assert_flag(duplicated_ids)
      self$man = assert_string(man, na.ok = TRUE)
    },

    format = function() {
      sprintf("<%s>", class(self)[1L])
    },

    print = function(...) {
      pv = self$param_set$values
      catf("%s with %i iterations", format(self), self$iters)
      catf(str_indent("* Instantiated:", self$is_instantiated))
      catf(str_indent("* Parameters:", as_short_string(pv, 1000L)))
    },

    instantiate = function(task) {
      task = assert_task(as_task(task))
      stratify = task$stratify
      groups = task$groups

      if (is.null(stratify)) {
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
        instance = private$.combine(lapply(stratify$row_id, private$.sample))
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


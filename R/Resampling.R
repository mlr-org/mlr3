#' @title Resampling Class
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
#' Predefined resamplings are stored in the [dictionary][mlr3misc::Dictionary] [mlr_resamplings],
#' e.g. [`cv`][mlr_resamplings_cv] or [`bootstrap`][mlr_resamplings_bootstrap].
#'
#'
#' @template param_id
#' @template param_param_set
#' @template param_man
#'
#' @section Stratification:
#' All derived classes support stratified sampling.
#' The stratification variables are assumed to be discrete and must be stored in the [Task] with column role `"stratum"`.
#' In case of multiple stratification variables, each combination of the values of the stratification variables forms a strata.
#'
#' First, the observations are divided into subpopulations based one or multiple stratification variables (assumed to be discrete), c.f. `task$strata`.
#'
#' Second, the sampling is performed in each of the `k` subpopulations separately.
#' Each subgroup is divided into `iter` training sets and `iter` validation sets by the derived `Resampling`.
#' These sets are merged based on their iteration number:
#' all training sets from all subpopulations with iteration 1 are combined, then all training sets with iteration 2, and so on.
#' Same is done for all validation sets.
#' The merged sets can be accessed via `$train_set(i)` and `$validation_set(i)`, respectively.
#' Note that this procedure can lead to set sizes that are slightly different from those
#' without stratification.
#'
#'
#' @section Grouping / Blocking:
#' All derived classes support grouping of observations.
#' The grouping variable is assumed to be discrete and must be stored in the [Task] with column role `"group"`.
#'
#' Observations in the same group are treated like a "block" of observations which must be kept together.
#' These observations either all go together into the training set or together into the validation set.
#'
#' The sampling is performed by the derived [Resampling] on the grouping variable.
#' Next, the grouping information is replaced with the respective row ids to generate training and validation sets.
#' The sets can be accessed via `$train_set(i)` and `$validation_set(i)`, respectively.
#'
#'
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
#' # Instantiate on penguins task
#' task = tsk("penguins")
#' r$instantiate(task)
#'
#' # Extract train/validation sets
#' train_set = r$train_set(1)
#' print(train_set)
#' intersect(train_set, r$validation_set(1))
#'
#' # Another example: 10-fold CV
#' r = rsmp("cv")$instantiate(task)
#' r$train_set(1)
#'
#' # Stratification
#' task = tsk("pima")
#' prop.table(table(task$truth())) # moderately unbalanced
#' task$col_roles$stratum = task$target_names
#'
#' r = rsmp("subsampling")
#' r$instantiate(task)
#' prop.table(table(task$truth(r$train_set(1)))) # roughly same proportion
Resampling = R6Class("Resampling",
  public = list(
    #' @template field_id
    id = NULL,

    #' @template field_param_set
    param_set = NULL,

    #' @field instance (any)\cr
    #'   During `instantiate()`, the instance is stored in this slot in an arbitrary format.
    #'   Note that if a grouping variable is present in the [Task], a [Resampling] may operate on the
    #'   group ids internally instead of the row ids (which may lead to confusion).
    #'
    #'   It is advised to not work directly with the `instance`, but instead only use the getters
    #'   `$train_set()` and `$validation_set()`.
    instance = NULL,

    #' @field task_hash (`character(1)`)\cr
    #'   The hash of the [Task] which was passed to `r$instantiate()`.
    task_hash = NA_character_,

    #' @field task_nrow (`integer(1)`)\cr
    #'   The number of observations of the [Task] which was passed to `r$instantiate()`.
    #'
    task_nrow = NA_integer_,

    #' @field duplicated_ids (`logical(1)`)\cr
    #'   If `TRUE`, duplicated rows can occur within a single training set or within a single validation set.
    #'   E.g., this is `TRUE` for Bootstrap, and `FALSE` for cross-validation.
    #'   Only used internally.
    duplicated_ids = NULL,

    #' @template field_man
    man = NULL,

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param duplicated_ids (`logical(1)`)\cr
    #'   Set to `TRUE` if this resampling strategy may have duplicated row ids in a single training set or validation set.
    #'
    #' Note that this object is typically constructed via a derived classes, e.g. [ResamplingCV] or [ResamplingHoldout].
    initialize = function(id, param_set = ps(), duplicated_ids = FALSE, man = NA_character_) {
      self$id = assert_string(id, min.chars = 1L)
      self$param_set = assert_param_set(param_set)
      self$duplicated_ids = assert_flag(duplicated_ids)
      self$man = assert_string(man, na.ok = TRUE)
    },

    #' @description
    #' Helper for print outputs.
    format = function() {
      sprintf("<%s>", class(self)[1L])
    },

    #' @description
    #' Printer.
    #' @param ... (ignored).
    print = function(...) {
      pv = self$param_set$values
      catf("%s with %i iterations", format(self), self$iters)
      catn(str_indent("* Instantiated:", self$is_instantiated))
      catn(str_indent("* Parameters:", as_short_string(pv, 1000L)))
    },

    #' @description
    #' Opens the corresponding help page referenced by field `$man`.
    help = function() {
      open_help(self$man)
    },

    #' @description
    #' Materializes fixed training and validation splits for a given task and stores them in `r$instance`
    #' in an arbitrary format.
    #'
    #' @param task ([Task])\cr
    #'   Task used for instantiation.
    #'
    #' @return
    #' Returns the object itself, but modified **by reference**.
    #' You need to explicitly `$clone()` the object beforehand if you want to keeps
    #' the object in its previous state.
    instantiate = function(task) {
      task = assert_task(as_task(task))
      strata = task$strata
      groups = task$groups

      if (is.null(strata)) {
        if (is.null(groups)) {
          instance = private$.sample(task$row_ids, task)
        } else {
          private$.groups = groups
          instance = private$.sample(unique(groups$group))
        }
      } else {
        if (!is.null(groups)) {
          stopf("Cannot combine stratification with grouping")
        }
        instance = private$.combine(lapply(strata$row_id, private$.sample, task = task))
      }

      self$instance = instance
      self$task_hash = task$hash
      self$task_nrow = task$nrow
      invisible(self)
    },

    #' @description
    #' Returns the row ids of the i-th training set.
    #'
    #' @param i (`integer(1)`)\cr
    #'   Iteration.
    #' @return (`integer()`) of row ids.
    train_set = function(i) {
      private$.get_set(private$.get_train, i)
    },

    #' @description
    #' Returns the row ids of the i-th validation set.
    #'
    #' @param i (`integer(1)`)\cr
    #'   Iteration.
    #' @return (`integer()`) of row ids.
    validation_set = function(i) {
      getter = if (exists(".get_validation", envir = private)) private$.get_validation else private$.get_test
      private$.get_set(getter, i)
    },

    #' @description
    #' Returns the row ids of the i-th validation set.
    #'
    #' @param i (`integer(1)`)\cr
    #'   Iteration.
    #' @return (`integer()`) of row ids.
    test_set = function(i) {
      .Deprecated(new = "validation_set", "The set 'test' has been renamed to 'validation'", package = "mlr3")
      self$validation_set(i)
    }
  ),

  active = list(
    #' @field is_instantiated (`logical(1)`)\cr
    #'   Is `TRUE` if the resampling has been instantiated.
    is_instantiated = function(rhs) {
      assert_ro_binding(rhs)
      !is.null(self$instance)
    },

    #' @template field_hash
    hash = function(rhs) {
      assert_ro_binding(rhs)
      if (!self$is_instantiated) {
        return(NA_character_)
      }
      calculate_hash(list(class(self), self$id, self$param_set$values, self$instance))
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

      if (is.null(private$.groups)) {
        return(ids)
      }

      private$.groups[list(ids), on = "group", allow.cartesian = TRUE][[1L]]
    }
  )
)


#' @export
as.data.table.Resampling = function(x, ...) { # nolint
  assert_resampling(x, instantiated = TRUE)
  iterations = seq_len(x$iters)

  tab = rbindlist(list(
    map_dtr(iterations, function(i) list(row_id = x$train_set(i)), .idcol = "iteration"),
    map_dtr(iterations, function(i) list(row_id = x$validation_set(i)), .idcol = "iteration")
  ), idcol = "set")
  set(tab, j = "set", value = factor(c("train", "validation")[tab$set], levels = c("train", "validation")))
  setkeyv(tab, c("set", "iteration"))[]
}

#' @export
format_list_item.Resampling = function(x, ...) { # nolint
  sprintf("<rsmp:%s[%i]>", x$id, x$iters)
}

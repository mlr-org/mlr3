#' @title Measure Class
#'
#' @include mlr_reflections.R
#'
#' @description
#' This is the abstract base class for measures like [MeasureClassif] and [MeasureRegr].
#'
#' Measures are classes tailored around two functions doing the work:
#'
#' 1. A function `$score()` which quantifies the performance by comparing the truth and predictions.
#' 2. A function `$aggregator()` which combines multiple performance scores returned by
#'    `$score()` to a single numeric value.
#'
#' In addition to these two functions, meta-information about the performance measure is stored.
#'
#' Predefined measures are stored in the [dictionary][mlr3misc::Dictionary] [mlr_measures],
#' e.g. [`classif.auc`][mlr_measures_classif.auc] or [`time_train`][mlr_measures_time_train].
#' Many of the measures in \pkg{mlr3} are implemented in \CRANpkg{mlr3measures} as ordinary functions.
#'
#' A guide on how to extend \CRANpkg{mlr3} with custom measures can be found in the [mlr3book](https://mlr3book.mlr-org.com).
#' @section Inheriting:
#' For some measures (such as confidence intervals from `mlr3inferr`) it is necessary that a measure
#' returns more than one value.
#' In such cases it is necessary to overwrite the public methods `$aggregate()` and/or `$score()` to return a named `numeric()`
#' where at least one of its names corresponds to the `id` of the measure itself.
#'
#' @section Weights:
#'
#' Many measures support observation weights, indicated by their property `"weights"`.
#' The weights are stored in the [Task] where the column role `weights_measure` needs to be assigned to a single numeric column.
#' The weights are automatically used if found in the task, this can be disabled by setting the field `use_weights` to `"ignore"`.
#' See the description of `use_weights` for more information.
#'
#' If the measure is set-up to use weights but the task does not have a designated `weights_measure` column, an unweighted version is calculated instead.
#' The weights do not necessarily need to sum up to 1, they are normalized by the measure if necessary.
#'
#' Most measures are so-called decomposable loss functions where a point-wise loss is computed and then either mean-aggregated or summed
#' over the test set.
#' For measures that do mean-aggregation, weights are typically used to compute the weighted mean, which normalizes weights to sum to 1.
#' Measures that use sum-aggregation do not normalize weights and instead multiply individual losses with the given weights.
#' See the documentation of specific measures for more details.
#'
#'
#' @template param_id
#' @template param_param_set
#' @template param_range
#' @template param_minimize
#' @template param_average
#' @template param_aggregator
#' @template param_task_type
#' @template param_predict_type
#' @template param_measure_properties
#' @template param_predict_sets
#' @template param_task_properties
#' @template param_packages
#' @template param_label
#' @template param_man
#' @param obs_loss (`function` or `NULL`)\cr
#'   The observation-wise loss function, e.g. [zero-one][mlr3measures::zero_one] for classification error.
#' @param trafo (`list()` or `NULL`)\cr
#'   An optional list with two elements, containing the transformation `"fn"` and its derivative `"deriv"`.
#'   The transformation function is the function that is applied after aggregating the pointwise losses, i.e.
#'   this requires an `$obs_loss` to be present. An example is `sqrt` for RMSE.
#'
#' @template seealso_measure
#' @export
Measure = R6Class("Measure",
  public = list(
    #' @template field_id
    id = NULL,

    #' @template field_label
    label = NULL,

    #' @template field_task_type
    task_type = NULL,

    #' @template field_param_set
    param_set = NULL,

    #' @field obs_loss (`function()` | `NULL`)
    #' Function to calculate the observation-wise loss.
    obs_loss = NULL,

    #' @field trafo (`list()` | `NULL`)
    #' `NULL` or a list with two elements:
    #' * `trafo`: the transformation function applied after aggregating
    #'   observation-wise losses (e.g. `sqrt` for RMSE)
    #' * `deriv`: The derivative of the `trafo`.
    trafo = NULL,

    #' @field predict_type (`character(1)`)\cr
    #' Required predict type of the [Learner].
    predict_type = NULL,

    #' @field check_prerequisites (`character(1)`)\cr
    #' How to proceed if one of the following prerequisites is not met:
    #'
    #' * wrong predict type (e.g., probabilities required, but only labels available).
    #' * wrong predict set (e.g., learner predicted on training set, but predictions of test set required).
    #' * task properties not satisfied (e.g., binary classification measure on multiclass task).
    #'
    #' Possible values are `"ignore"` (just return `NaN`) and `"warn"` (default, raise a warning before returning `NaN`).
    check_prerequisites = "warn",

    #' @field task_properties (`character()`)\cr
    #' Required properties of the [Task].
    task_properties = NULL,

    #' @field range (`numeric(2)`)\cr
    #' Lower and upper bound of possible performance scores.
    range = NULL,

    #' @field minimize (`logical(1)`)\cr
    #' If `TRUE`, good predictions correspond to small values of performance scores.
    minimize = NULL,

    #' @template field_packages
    packages = NULL,

    #' @template field_man
    man = NULL,

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' Note that this object is typically constructed via a derived classes, e.g. [MeasureClassif] or [MeasureRegr].
    initialize = function(id, task_type = NA, param_set = ps(), range = c(-Inf, Inf), minimize = NA, average = "macro",
      aggregator = NULL, obs_loss = NULL, properties = character(), predict_type = "response",
      predict_sets = "test", task_properties = character(), packages = character(),
      label = NA_character_, man = NA_character_, trafo = NULL) {

      self$id = assert_string(id, min.chars = 1L)
      self$label = assert_string(label, na.ok = TRUE)
      self$task_type = task_type
      self$param_set = assert_param_set(param_set)
      self$range = assert_range(range)
      self$minimize = assert_flag(minimize, na.ok = TRUE)
      self$average = average
      private$.aggregator = assert_function(aggregator, null.ok = TRUE)
      self$obs_loss = assert_function(obs_loss, null.ok = TRUE)
      self$trafo = assert_list(trafo, len = 2L, types = "function", null.ok = TRUE)
      if (!is.null(self$trafo)) {
        assert_permutation(names(trafo), c("fn", "deriv"))
      }

      if (!is_scalar_na(task_type)) {
        assert_choice(task_type, mlr_reflections$task_types$type)
        assert_subset(properties, mlr_reflections$measure_properties[[task_type]])
        assert_choice(predict_type, names(mlr_reflections$learner_predict_types[[task_type]]))
        assert_subset(task_properties, mlr_reflections$task_properties[[task_type]])
      } else {
        assert_subset(properties, unique(unlist(mlr_reflections$measure_properties, use.names = FALSE)))
      }

      self$properties = unique(properties)

      if ("weights" %in% properties) {
        self$use_weights = "use"
      } else if ("requires_no_prediction" %in% properties) {
        self$use_weights = "ignore"
      } else {
        self$use_weights = "error"
      }

      self$predict_type = predict_type
      self$predict_sets = predict_sets
      self$task_properties = task_properties
      self$packages = union("mlr3", assert_character(packages, any.missing = FALSE, min.chars = 1L))
      self$man = assert_string(man, na.ok = TRUE)

      check_packages_installed(packages, msg = sprintf("Package '%%s' required but not installed for Measure '%s'", id))
    },

    #' @description
    #' Helper for print outputs.
    #' @param ... (ignored).
    format = function(...) {
      sprintf("<%s:%s>", class(self)[1L], self$id)
    },

    #' @description
    #' Printer.
    #' @param ... (ignored).
    print = function(...) {
      msg_h = if (is.null(self$label) || is.na(self$label)) "" else paste0(": ", self$label)
      msg_properties = if (length(self$properties)) self$properties else "-"
      cat_cli({
        cli_h1("{.cls {class(self)[1L]}} ({self$id}){msg_h}")
        cli_li("Packages: {.pkg {self$packages}}")
        cli_li("Range: [{self$range[1L]}, {self$range[2L]}]")
        cli_li("Minimize: {.val {self$minimize}}")
        cli_li("Average: {self$average}")
        cli_li("Parameters: {as_short_string(self$param_set$values, 1000L)}")
        cli_li("Properties: {msg_properties}")
        cli_li("Predict type: {self$predict_type}")
        cli_li("Predict sets: {self$predict_sets}")
        cli_li("Aggregator: {if (is.null(self$aggregator)) 'mean()' else '[user-defined]'}")
      })
    },

    #' @description
    #' Opens the corresponding help page referenced by field `$man`.
    help = function() {
      open_help(self$man)
    },

    #' @description
    #' Takes a [Prediction] (or a list of [Prediction] objects named with valid `predict_sets`)
    #' and calculates a numeric score.
    #' If the measure if flagged with the properties `"requires_task"`, `"requires_learner"`,
    #' `"requires_model"` or `"requires_train_set"`, you must additionally
    #' pass the respective [Task], the (trained) [Learner] or the training set indices.
    #' This is handled internally during [resample()]/[benchmark()].
    #'
    #' @param prediction ([Prediction] | named list of [Prediction]).
    #'
    #' @param task ([Task]).
    #'
    #' @param learner ([Learner]).
    #'
    #' @param train_set (`integer()`).
    #'
    #' @return `numeric(1)`.
    score = function(prediction, task = NULL, learner = NULL, train_set = NULL) {
      assert_scorable(self, task = task, learner = learner, prediction = prediction)
      properties = self$properties
      assert_prediction(prediction, null.ok = "requires_no_prediction" %nin% properties)

      # check should be added to assert_measure()
      # except when the checks are superfluous for rr$score() and bmr$score()
      # these checks should be added bellow
      if ("requires_task" %chin% properties && is.null(task)) {
        stopf("Measure '%s' requires a task", self$id)
      }

      if ("requires_learner" %chin% properties && is.null(learner)) {
        stopf("Measure '%s' requires a learner", self$id)
      }

      if (!is_scalar_na(self$task_type) && self$task_type != prediction$task_type) {
        stopf("Measure '%s' incompatible with task type '%s'", self$id, prediction$task_type)
      }

      if ("requires_train_set" %chin% properties && is.null(train_set)) {
        stopf("Measure '%s' requires the train_set", self$id)
      }

      score_single_measure(self, task, learner, train_set, prediction)
    },

    #' @description
    #' Aggregates multiple performance scores into a single score, e.g. by using the `aggregator`
    #' function of the measure.
    #'
    #' @param rr [ResampleResult].
    #'
    #' @return `numeric(1)`.
    aggregate = function(rr) {
      switch(self$average,
        "macro_weighted" = {
          aggregator = self$aggregator %??% weighted.mean
          tab = score_measures(rr, list(self), reassemble = FALSE, view = get_private(rr)$.view,
            iters = get_private(rr$resampling)$.primary_iters)
          # score_measures constructs both .weights and .samples, since it can work with multiple measures and hence
          # does not depend on an individual measure's `use_weights` setting.
          set_names(aggregator(tab[[self$id]], if (self$use_weights == "use") tab$.weights else tab$.samples), self$id)
        },
        "macro" = {
          aggregator = self$aggregator %??% mean
          tab = score_measures(rr, list(self), reassemble = FALSE, view = get_private(rr)$.view,
            iters = get_private(rr$resampling)$.primary_iters)
          set_names(aggregator(tab[[self$id]]), self$id)
        },
        "micro" = {
          if (is.null(get_private(rr$resampling)$.primary_iters)) {
            self$score(rr$prediction(self$predict_sets), task = rr$task, learner = rr$learner)
          } else {
            prediction = do.call(c, rr$predictions(self$predict_sets)[get_private(rr$resampling)$.primary_iters])
            self$score(prediction, task = rr$task, learner = rr$learner)
          }
        },
        "custom" =  {
          if (!is.null(get_private(rr$resampling)$.primary_iters) && "primary_iters" %nin% self$properties &&
              !test_permutation(get_private(rr$resampling)$.primary_iters, seq_len(rr$resampling$iters))) {
            stopf("Resample result has non-NULL primary_iters, but measure '%s' cannot handle them", self$id)
          }
          private$.aggregator(rr)
        }
      )
    }
  ),

  active = list(
    #' @template field_predict_sets
    predict_sets = function(rhs) {
      if (!missing(rhs)) {
        private$.predict_sets = assert_subset(rhs, mlr_reflections$predict_sets, empty.ok = "requires_no_prediction" %chin% self$properties)
      }
      private$.predict_sets
    },

    #' @field hash (`character(1)`)\cr
    #' Hash (unique identifier) for this object.
    #' The hash is calculated based on the id, the parameter settings, predict sets and the `$score`, `$average`, `$aggregator`, `$obs_loss`, `$trafo` method.
    #' Measure can define additional fields to be included in the hash by setting the field `$.extra_hash`.
    hash = function(rhs) {
      assert_ro_binding(rhs)
      calculate_hash(class(self), self$id, self$param_set$values, private$.score,
        private$.average, private$.aggregator, self$obs_loss, self$trafo,
        self$predict_sets, mget(private$.extra_hash, envir = self), private$.use_weights)
    },

    #' @field properties (`character()`)\cr
    #' Properties of this measure.
    properties = function(rhs) {
      if (!missing(rhs)) {
        props = if (is.na(self$task_type)) unique(unlist(mlr_reflections$measure_properties, use.names = FALSE)) else mlr_reflections$measure_properties[[self$task_type]]
        private$.properties = assert_subset(rhs, props)
      } else {
        private$.properties
      }
    },

    #' @field average (`character(1)`)\cr
    #' Method for aggregation:
    #'
    #' * `"micro"`:
    #'   All predictions from multiple resampling iterations are first combined into a single [Prediction] object.
    #'   Next, the scoring function of the measure is applied on this combined object, yielding a single numeric score.
    #' * `"macro"`:
    #'   The scoring function is applied on the [Prediction] object of each resampling iterations,
    #'   each yielding a single numeric score.
    #'   Next, the scores are combined with the `aggregator` function to a single numerical score.
    #' * `"macro_weighted"`:
    #'   The scoring function is applied on the [Prediction] object of each resampling iterations,
    #'   each yielding a single numeric score.
    #'   Next, the scores are combined with the `aggregator` function to a single numerical score.
    #'   The scores are weighted by the total sample weights (if present, and if `$use_weights` is set to `"use"`),
    #'   or the number of samples in each resampling iteration.
    #' * `"custom"`:
    #'   The measure comes with a custom aggregation method which directly operates on a [ResampleResult].
    average = function(rhs) {
      if (!missing(rhs)) {
        private$.average = assert_choice(rhs, c("micro", "macro", "custom", "macro_weighted"))
      } else {
        private$.average
      }
    },

    #' @field aggregator (`function()`)\cr
    #' Function to aggregate scores computed on different resampling iterations.
    aggregator = function(rhs) {
      if (!missing(rhs)) {
        private$.aggregator = assert_function(rhs, null.ok = TRUE)
      } else {
        private$.aggregator
      }
    },

    #' @field use_weights (`character(1)`)\cr
    #' How to handle weights.
    #' Settings are `"use"`, `"ignore"`, and `"error"`.
    #'
    #' * `"use"`: Weights are used automatically if found in the task, as supported by the measure.
    #' * `"ignore"`: Weights are ignored.
    #' * `"error"`: throw an error if weights are present in the training `Task`.
    #'
    #' For measures with the property `"weights"`, this is initialized as `"use"`.
    #' For measures with the property `"requires_no_prediction"`, this is initialized as `"ignore"`.
    #' For measures that have neither of the properties, this is initialized as `"error"`.
    #' The latter behavior is to avoid cases where a user erroneously assumes that a measure supports weights when it does not.
    #' For measures that do not support weights, `use_weights` needs to be set to `"ignore"` if tasks with weights should be handled (by dropping the weights).
    use_weights = function(rhs) {
      if (!missing(rhs)) {
        private$.use_weights = assert_choice(rhs, c(if ("weights" %chin% self$properties) "use", "ignore", "error"))
      } else {
        private$.use_weights
      }
    }
  ),

  private = list(
    .properties = character(),
    .predict_sets = NULL,
    .extra_hash = character(),
    .average = NULL,
    .aggregator = NULL,
    .use_weights = NULL,
    .score = function(prediction, task, weights, ...) {
      stop("abstract method")
    }
  )
)


#' @title Workhorse function to calculate a single score
#'
#' @description
#' Assumes that learner is reassembled (if required).
#' It is usually a good idea to exploit lazy evaluation while calling this function
#' to avoid unnecessary allocations.
#'
#' @param measure ([Measre]).
#' @param task ([Measre]).
#' @param learner ([Measre]).
#' @param train_set (`intger()`).
#' @param prediction ([Prediction] | [PredictionData]).
#'
#' @return (`numeric()`).
#' @noRd
score_single_measure = function(measure, task, learner, train_set, prediction) {
  if (!length(measure$predict_sets)) {
    score = get_private(measure)$.score(
      prediction = NULL, task = task, learner = learner, train_set = train_set
    )
    return(score)
  }

  # merge multiple predictions (on different predict sets) to a single one
  if (is.list(prediction)) {
    ii = match(measure$predict_sets, names(prediction))
    if (anyMissing(ii)) {
      # TODO lgr$debug()
      return(NaN)
    }
    prediction = do.call(c, prediction[ii])
  }

  # convert pdata to regular prediction
  prediction = as_prediction(prediction, check = FALSE)

  if (is.null(prediction) && length(measure$predict_sets)) {
    return(NaN)
  }

  if (!is_scalar_na(measure$predict_type) && measure$predict_type %nin% prediction$predict_types) {
    # TODO lgr$debug()
    return(NaN)
  }

  if (!is.null(task) && any(measure$task_properties %nin% task$properties)) {
    # TODO lgr$debug()
    return(NaN)
  }

  get_private(measure)$.score(prediction = prediction, task = task, learner = learner, train_set = train_set,
    weights = if (measure$use_weights == "use") prediction$weights)
}

#' @title Workhorse function to calculate multiple scores
#'
#' @description
#' Converts `obj` from [ResampleResult] or [BenchmarkResult] to a `data.table`.
#' Automatically reassembles learners if needed.
#' Uses lazy evaluation to avoid querying the `train_set` while calling
#' [score_single_measure()].
#'
#' @param obj ([ResampleResult] | [BenchmarkResult]).
#' @param measures (list of [Measure]).
#' @param iters (`integer()` or `NULL`)
#' To which iterations of the resample result to restrict the scoring.
#'
#' @return (`data.table()`) with added score columns.
#'
#' @noRd
score_measures = function(obj, measures, reassemble = TRUE, view = NULL, iters = NULL) {
  reassemble_learners = reassemble ||
    some(measures, function(m) any(c("requires_learner", "requires_model") %chin% m$properties))
  tab = get_private(obj)$.data$as_data_table(view = view, reassemble_learners = reassemble_learners, convert_predictions = FALSE)

  set(tab, j = ".samples", value = map_dbl(tab$prediction, function(x) length(x$test$row_ids)))
  if (length(tab$prediction) && "weights" %chin% names(tab$prediction[[1L]]$test)) {
    set(tab, j = ".weights", value = map_dbl(tab$prediction, function(x) sum(x$test$weights)))
  } else {
    set(tab, j = ".weights", value = tab$.samples)
  }


  if (!is.null(iters)) {
    tab = tab[list(iters), on = "iteration"]
  }

  tmp = unique(tab, by = c("task_hash", "learner_hash"))[, c("task", "learner"), with = FALSE]

  for (measure in measures) {
    pmap(tmp, assert_scorable, measure = measure)

    score = pmap_dbl(tab[, c("task", "learner", "resampling", "iteration", "prediction"), with = FALSE],
      function(task, learner, resampling, iteration, prediction) {
        score_single_measure(measure, task, learner, train_set = resampling$train_set(iteration), prediction)
      }
    )
    set(tab, j = measure$id, value = score)
  }

  tab[]
}


# format_list_item.Measure = function(x, ...) { # nolint
#   sprintf("<msr:%s>", x$id)
# }

#' @export
rd_info.Measure = function(obj, ...) { # nolint
  x = c("",
    sprintf("* Task type: %s", rd_format_string(obj$task_type)),
    sprintf("* Range: %s", rd_format_range(obj$range[1L], obj$range[2L])),
    sprintf("* Minimize: %s", obj$minimize),
    sprintf("* Average: %s", obj$average),
    sprintf("* Required Prediction: %s", rd_format_string(obj$predict_type)),
    sprintf("* Required Packages: %s", rd_format_packages(obj$packages))
  )
  paste(x, collapse = "\n")
}

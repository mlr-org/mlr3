#' @title Similarity Measure
#'
#' @include Measure.R
#'
#' @description
#' This measure specializes [Measure] for measures quantifying the similarity of
#' sets of selected features.
#' To calculate similarity measures, the [Learner] must have the property
#' `"selected_features"`.
#'
#' * `task_type` is set to `NA_character_`.
#' * `average` is set to `"custom"`.
#'
#' Predefined measures can be found in the [dictionary][mlr3misc::Dictionary]
#' [mlr_measures], prefixed with `"sim."`.
#'
#' @template param_id
#' @template param_param_set
#' @template param_range
#' @template param_minimize
#' @template param_average
#' @template param_aggregator
#' @template param_predict_type
#' @template param_measure_properties
#' @template param_predict_sets
#' @template param_task_properties
#' @template param_packages
#' @template param_label
#' @template param_man
#'
#' @template seealso_measure
#' @export
#' @examples
#' task = tsk("penguins")
#' learners = list(
#'   lrn("classif.rpart", maxdepth = 1, id = "r1"),
#'   lrn("classif.rpart", maxdepth = 2, id = "r2")
#' )
#' resampling = rsmp("cv", folds = 3)
#' grid = benchmark_grid(task, learners, resampling)
#' bmr = benchmark(grid, store_models = TRUE)
#' bmr$aggregate(msrs(c("classif.ce", "sim.jaccard")))
MeasureSimilarity = R6Class("MeasureSimilarity",
  inherit = Measure,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(id, param_set = ps(), range, minimize = NA, average = "macro", aggregator = NULL, properties = character(), predict_type = NA_character_,
      task_properties = character(), packages = character(), label = NA_character_, man = NA_character_) {
      super$initialize(id, task_type = NA_character_, param_set = param_set, range = range, minimize = minimize, average = "custom", aggregator = aggregator,
        properties = c("requires_model", "requires_no_prediction", properties), predict_type = predict_type, predict_sets = NULL,
        task_properties = task_properties, packages = packages, label = label, man = man)
    }
  ),

  private = list(
    .score = function(prediction, ...) {
      NA_real_
    }
  )
)

#' @title Measure Class
#'
#' @usage NULL
#' @format [R6::R6Class] object.
#'
#' @description
#' This is the abstract base class for measures like [MeasureClassif] and [MeasureRegr].
#' Predefined measures are stored in [mlr_measures].
#'
#' @templateVar MeasureClass Base
#' @template Measure
#'
#' @family Measure
#' @export
Measure = R6Class("Measure",
  cloneable = FALSE,
  public = list(
    task_type = NULL,
    predict_type = NULL,
    task_properties = NULL,
    range = NULL,
    minimize = NULL,
    packages = NULL,
    aggregate = function(rr) mean(rr$performance(self$id)),

    initialize = function(id, task_type, range, minimize, predict_type = "response", task_properties = character(0L), packages = character(0L)) {
      self$id = assert_id(id)
      self$task_type = task_type
      self$range = assert_range(range)
      self$minimize = assert_flag(minimize)

      if (!is_scalar_na(task_type)) {
        assert_choice(task_type, mlr_reflections$task_types)
        assert_choice(predict_type, mlr_reflections$predict_types[[task_type]])
      }
      self$predict_type = predict_type
      self$task_properties = assert_sorted_subset(task_properties, mlr_reflections$task_properties[[task_type]])
      self$packages = assert_set(packages)
    },

    format = function() {
      sprintf("<%s:%s>", class(self)[1L], self$id)
    },

    print = function() {
      catf(format(self))
      catf(str_indent("Packages:", self$packages))
      catf(str_indent("Range:", sprintf("[%g, %g]", self$range[1L], self$range[2L])))
      catf(str_indent("Minimize:", self$minimize))
      catf(str_indent("Predict type:", self$predict_type))
    }
  ),

  private = list(
    .calculate_hash = function() {
      hash(list(class(self), private$.id, as.character(body(self$calculate))))
    }
  )
)

add_id_hash(Measure)

#' @export
MeasureAbstractCI = R6Class("MeasureCI",
  inherit = Measure,
  public = list(
    #' @field resamplings (`character()`)\cr
    #' On which resamplings this method can operate.
    resamplings = NULL,
    measure = NULL,
    initialize = function(measure = NULL, param_set = ps(), packages = character(), resamplings) {
      self$measure = if (test_string(measure)) {
        msr(measure)
      } else {
        assert(
          check_class(measure, "Measure"),
          check_false(inherits(measure, "MeasureCI")),
          check_function(measure$obs_loss),
          combine = "and",
          .var.name = "Argument measure must be a scalar Measure with a pointwise loss function"
        )
        measure
      }

      param_set = c(param_set,
        ps(
          alpha = p_dbl(lower = 0, upper = 1, init = 0.05, tags = "required"),
          within_range = p_lgl(init = TRUE, tags = "required")
        )
      )

      self$resamplings = assert_character(resamplings)

      super$initialize(
        id = self$measure$id,
        param_set = param_set,
        range = self$measure$range,
        minimize = self$measure$minimize,
        average = "custom",
        properties = self$measure$properties,
        predict_type = self$measure$predict_type,
        packages = unique(c(self$measure$packages), packages)
      )
    },
    aggregate = function(rr) {
      measure = if (is.null(self$measure)) {
        msr(mlr_reflections$default_measures[[rr$task_type]])
      } else {
        self$measure
      }

      if (!is.na(self$resamplings) && !test_multi_class(rr$resampling, self$resamplings)) {
        stopf("CI for Measure '%s'")
      }


      
      param_vals = self$param_set$get_values()
      tbl = rr$obs_loss(self$measure)
      ci = private$.ci(tbl, rr, param_vals)
      if (param_vals$within_range) {
        ci[2:3] = pmin(pmax(ci[2:3], self$range[1L]), self$range[2L])
      }
      set_names(ci, c(self$id, paste0(measure$id, ".", c("lower", "upper"))))
    }
  ),
  private = list(
    .ci = function(tbl, rr, param_vals) {
      stopf("Abstract method.")
    }
  )
)

#' @export
MeasureCI = R6Class("Measure",
  inherit = MeasureAbstractCI,
  private = list(
    .ci = function(tbl, rr, param_vals) {
    # TODO: pick a default CI method if available, possibly via reflections
    }
  )
)

#' @include mlr_measures.R
mlr_measures$add("ci", MeasureCI, .prototype_args = list(measure = "classif.acc", id = "ci"))

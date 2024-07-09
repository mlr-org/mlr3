#' @export
MeasureCIHoldout = R6Class("MeasureCIHoldout",
  inherit = MeasureAbstractCI,
  public = list(
    initialize = function(measure) {
      super$initialize(
        measure = measure,
        resamplings = "ResamplingHoldout"
      )
    }
  ),
  private = list(
    .ci = function(tbl, rr, param_vals) {
      losses = tbl[, self$measure$id, with = FALSE][[1L]]
      estimate = mean(losses)
      se = sd(losses) / sqrt(nrow(tbl))
      z = qnorm(1 - param_vals$alpha / 2)

      c(estimate, estimate - se * z, estimate + se * z)
    }
  )
)

#' @include mlr_measures.R
mlr_measures$add("ci.holdout", MeasureCIHoldout, .prototype_args = list(measure = "classif.acc", id = "ci.holdout"))

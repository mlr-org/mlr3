#' @title Peak Regression Task Generator
#'
#' @name mlr_task_generators_peak
#' @include TaskGenerator.R
#'
#' @description
#' A [TaskGenerator] for the peak task in [mlbench::mlbench.peak()].
#'
#' @templateVar id peak
#' @template task_generator
#'
#' @template seealso_task_generator
#' @export
#' @examples
#' generator = tgen("peak", d = 5)
#' task = generator$generate(200)
#' str(task$data())
TaskGeneratorPeak = R6Class(
  "TaskGeneratorPeak",
  inherit = TaskGenerator,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      ps = ps(
        d = p_int(lower = 1, default = 20L)
      )

      super$initialize(
        id = "peak",
        "regr",
        "mlbench",
        ps,
        label = "Peak Regression",
        man = "mlr3::mlr_task_generators_peak"
      )
    }
  ),

  private = list(
    .generate_obj = function(n) {
      invoke(
        mlbench::mlbench.peak,
        n = n,
        .args = self$param_set$values,
        .opts = allow_partial_matching
      )
    },

    .generate = function(n) {
      obj = invoke(
        mlbench::mlbench.peak,
        n = n,
        .args = self$param_set$values
      )
      colnames(obj$x) = c(
        sprintf("x%0i", seq_len(ncol(obj$x)))
      )
      data = insert_named(as.data.table(obj$x), list(y = obj$y))
      TaskRegr$new(sprintf("%s_%i", self$id, n), data, target = "y")
    }
  )
)

#' @include mlr_task_generators.R
mlr_task_generators$add("peak", function() TaskGeneratorPeak$new())

#' @title Subsampling Resampling
#'
#' @name mlr_resamplings_subsampling
#' @include Resampling.R
#'
#' @description
#' Splits data `repeats` (default: 30) times into training and test set
#' with a ratio of `ratio` (default: 2/3) observations going into the training set.
#'
#' @templateVar id holdout
#' @template section_dictionary_resampling
#'
#' @section Parameters:
#' * `repeats` (`integer(1)`)\cr
#'   Number of repetitions.
#' * `ratio` (`numeric(1)`)\cr
#'   Ratio of observations to put into the training set.
#'
#' @references
#' `r format_bib("bischl_2012")`
#'
#' @template seealso_resampling
#' @export
#' @examples
#' # Create a task with 10 observations
#' task = tsk("penguins")
#' task$filter(1:10)
#'
#' # Instantiate Resampling
#' subsampling = rsmp("subsampling", repeats = 2, ratio = 0.5)
#' subsampling$instantiate(task)
#'
#' # Individual sets:
#' subsampling$train_set(1)
#' subsampling$test_set(1)
#'
#' # Disjunct sets:
#' intersect(subsampling$train_set(1), subsampling$test_set(1))
#'
#' # Internal storage:
#' subsampling$instance$train # list of index vectors
ResamplingSubsampling = R6Class("ResamplingSubsampling", inherit = Resampling,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      ps = ps(
        ratio   = p_dbl(0, 1, tags = "required"),
        repeats = p_int(1, tags = "required")
      )
      ps$values = list(repeats = 30L, ratio = 2 / 3)

      super$initialize(id = "subsampling", param_set = ps, man = "mlr3::mlr_resamplings_subsampling")
    }
  ),

  active = list(
    #' @template field_iters
    iters = function(rhs) {
      assert_ro_binding(rhs)
      as.integer(self$param_set$values$repeats)
    }
  ),

  private = list(
    .sample = function(ids, ...) {
      pv = self$param_set$values
      n = length(ids)
      nr = round(n * pv$ratio)

      train = replicate(pv$repeats, sample.int(n, nr), simplify = FALSE)
      list(train = train, row_ids = ids)
    },

    .get_train = function(i) {
      self$instance$row_ids[self$instance$train[[i]]]
    },

    .get_test = function(i) {
      self$instance$row_ids[-self$instance$train[[i]]]
    },

    .combine = function(instances) {
      Reduce(function(lhs, rhs) {
        n = length(lhs$row_ids)
        list(train =
          Map(function(x, y) c(x, y + n), lhs$train, rhs$train),
        row_ids = c(lhs$row_ids, rhs$row_ids)
        )
      }, instances)
    }
  )
)

#' @include mlr_resamplings.R
mlr_resamplings$add("subsampling", ResamplingSubsampling)

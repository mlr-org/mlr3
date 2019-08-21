#' @title Subsampling Resampling
#'
#' @usage NULL
#' @aliases mlr_resamplings_subsampling
#' @format [R6::R6Class] inheriting from [Resampling].
#' @include Resampling.R
#'
#' @section Construction:
#' ```
#' ResamplingSubsampling$new()
#' mlr_resamplings$get("subsampling")
#' rsmp("subsampling")
#' ```
#'
#' @description
#' Splits data `repeats` (default: 30) times into training and test set
#' with a ratio of `ratio` (default: 2/3) observations going into the training set.
#'
#' @section Fields:
#' See [Resampling].
#'
#' @section Methods:
#' See [Resampling].
#'
#' @template seealso_resampling
#' @export
#' @examples
#' # Create a task with 10 observations
#' task = tsk("iris")
#' task$filter(1:10)
#'
#' # Instantiate Resampling
#' rss = rsmp("subsampling", repeats = 2, ratio = 0.5)
#' rss$instantiate(task)
#'
#' # Individual sets:
#' rss$train_set(1)
#' rss$test_set(1)
#' intersect(rss$train_set(1), rss$test_set(1))
#'
#' # Internal storage:
#' rss$instance$train # list of index vectors
ResamplingSubsampling = R6Class("ResamplingSubsampling", inherit = Resampling,
  public = list(
    initialize = function() {
      super$initialize(
        id = "subsampling",
        param_set = ParamSet$new(params = list(
          ParamUty$new("stratify", default = NULL),
          ParamInt$new("repeats", lower = 1, tags = "required"),
          ParamDbl$new("ratio", lower = 0, upper = 1, tags = "required"))
        ),
        param_vals = list(repeats = 30L, ratio = 2 / 3)
      )
    }
  ),

  active = list(
    iters = function() {
      self$param_set$values$repeats
    }
  ),

  private = list(
    .sample = function(ids) {
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

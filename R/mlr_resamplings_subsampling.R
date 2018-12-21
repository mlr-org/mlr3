#' @title Subsampling Resampling
#' @format [R6Class] object
#' @name mlr_resamplings_subsampling
#' @format [R6::R6Class] inheriting from [Resampling].
#'
#' @description
#' `repeats` times repeated splits into training and test set with a `ratio` ratio of training observations.
#'
#' @export
#' @include Resampling.R
#' @references [HTML help page](https://mlr3.mlr-org.com/reference/mlr_resamplings_subsampling.html)
#' @examples
#' # Create a task with 10 observations
#' task = mlr_tasks$get("iris")
#' task$filter(1:10)
#'
#' # Instantiate Resampling
#' rss = mlr_resamplings$get("subsampling")
#' rss$param_vals = list(repeats = 2, ratio = 0.5)
#' rss$instantiate(task)
#'
#' # Individual sets:
#' rss$train_set(1)
#' rss$test_set(1)
#' intersect(rss$train_set(1), rss$test_set(1))
#'
#' # Internal storage:
#' rss$instance$train # list of bit vectors
ResamplingSubsampling = R6Class("ResamplingSubsampling", inherit = Resampling,
  public = list(
    initialize = function(id = "subsampling") {
      super$initialize(
        id = id,
        param_set = ParamSet$new(params = list(
            ParamInt$new("repeats", lower = 1, tags = "required"),
            ParamDbl$new("ratio", lower = 0, upper = 1, tags = "required"))
        ),
        param_vals = list(repeats = 30L, ratio = 0.67)
      )
      self$has_duplicates = FALSE
    },

    instantiate = function(task, ...) {
      assert_task(task)
      private$.instantiate(instantiate_subsampling(task, self$param_vals$ratio, self$param_vals$repeats, stratify = self$stratify))
    },

    train_set = function(i) {
      i = assert_resampling_index(self, i)
      self$instance$row_ids[bit::as.which(self$instance$train[[i]])]
    },

    test_set = function(i) {
      i = assert_resampling_index(self, i)
      self$instance$row_ids[bit::as.which(!self$instance$train[[i]])]
    }
  ),

  active = list(
    iters = function() {
      self$param_vals$repeats
    }
  )
)


#' @include mlr_resamplings.R
mlr_resamplings$add("subsampling", ResamplingSubsampling)



resample_subsampling = function(ids, ratio, repeats) {
  n = length(ids)
  nr = round(n * ratio)

  train = replicate(repeats,
    bit::as.bit(replace(logical(n), sample.int(n, nr), TRUE)),
    simplify = FALSE)
  list(train = train, row_ids = ids)
}

instantiate_subsampling = function(task, ratio, repeats, stratify = character(0L)) {
  if (length(stratify) == 0L) {
    res = resample_subsampling(task$row_ids[[1L]], ratio, repeats)
  } else {
    grps = stratify_groups(task, stratify = stratify)
    res = lapply(grps$..row_id, resample_subsampling, ratio = ratio, repeats = repeats)
    res = Reduce(function(lhs, rhs) { list(train = Map(c, lhs$train, rhs$train), row_ids = c(lhs$row_ids , rhs$row_ids)) }, res)
  }
  res
}

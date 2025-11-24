#' @title Manually Partition into Training, Test and Validation Set
#'
#' @description
#' Creates a split of the row ids of a [Task] into a training and a test set, and optionally a validation set.
#'
#' @param task ([Task])\cr
#'   Task to operate on.
#' @param ratio (`numeric()`)\cr
#'   Ratio of observations to put into the training set.
#'   If a 2 element vector is provided, the first element is the ratio for the training set, the second element is the ratio for the test set.
#'   The validation set will contain the remaining observations.
#' @export
#' @examples
#' # regression task partitioned into training and test set
#' task = tsk("california_housing")
#' split = partition(task, ratio = 0.5)
#' data = data.frame(
#'   y = c(task$truth(split$train), task$truth(split$test)),
#'   split = rep(c("train", "predict"), lengths(split[c("train", "test")]))
#' )
#' boxplot(y ~ split, data = data)
#'
#' # classification task partitioned into training, test and validation set
#' task = tsk("pima")
#' split = partition(task, c(0.66, 0.14))
partition = function(task, ratio = 0.67) {
  assert_numeric(ratio, min.len = 1L, max.len = 2L)
  UseMethod("partition")
}

#' @rdname partition
#' @export
partition.Task = function(task, ratio = 0.67) {
  task = task$clone(deep = TRUE)
  if (sum(ratio) >= 1) {
    error_input("Sum of 'ratio' must be smaller than 1")
  }

  if (length(ratio) == 1L) {
    ratio[2L] = 1 - ratio
  } else {
    ratio[3L] = 1 - (ratio[1L] + ratio[2L])
  }

  r1 = rsmp("holdout", ratio = ratio[1L])$instantiate(task)
  task$row_roles$use = r1$test_set(1L)
  r2 = rsmp("holdout", ratio = ratio[2L] / (1 - ratio[1L]))$instantiate(task)

  list(
    train = r1$train_set(1L),
    test = r2$train_set(1L),
    validation = r2$test_set(1L)
  )
}

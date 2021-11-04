#' @title Manually Partition into Training and Test Set
#'
#' @description
#' Creates a split of the row ids of a [Task] into a training set and a test set while
#' optionally stratifying on the target column.
#'
#' For more complex partitions, see the example.
#'
#' @param task ([Task])\cr
#'   Task to operate on.
#' @param ratio (`numeric(1)`)\cr
#'   Ratio of observations to put into the training set.
#' @param stratify (`logical(1)`)\cr
#'   If `TRUE`, stratify on the target variable.
#'   For regression tasks, the target variable is first cut into `bins` bins.
#'   See `Task$add_strata()`.
#' @param ... (any)\cr
#'   Additional arguments, currently not used.
#' @export
#' @examples
#' # regression task
#' task = tsk("boston_housing")
#'
#' # roughly equal size split while stratifying on the binned response
#' split = partition(task, ratio = 0.5)
#' data = data.frame(
#'   y = c(task$truth(split$train), task$truth(split$test)),
#'   split = rep(c("train", "predict"), lengths(split))
#' )
#' boxplot(y ~ split, data = data)
#'
#'
#' # classification task
#' task = tsk("pima")
#' split = partition(task)
#'
#' # roughly same distribution of the target label
#' prop.table(table(task$truth()))
#' prop.table(table(task$truth(split$train)))
#' prop.table(table(task$truth(split$test)))
#'
#'
#' # splitting into 3 disjunct sets, using ResamplingCV and stratification
#' task = tsk("iris")
#' task$set_col_roles(task$target_names, add_to = "stratum")
#' r = rsmp("cv", folds = 3)$instantiate(task)
#'
#' sets = lapply(1:3, r$train_set)
#' lengths(sets)
#' prop.table(table(task$truth(sets[[1]])))
partition = function(task, ratio = 0.67, stratify = TRUE, ...) {
  assert_task(task)
  assert_number(ratio, lower = 0, upper = 1)
  assert_flag(stratify)

  UseMethod("partition")
}


#' @param bins (`integer(1)`)\cr
#'   Number of bins to cut the target variable into for stratification.
#' @export
#' @rdname partition
partition.TaskRegr = function(task, ratio = 0.67, stratify = TRUE, bins = 3L, ...) { # nolint
  if (stratify) {
    task = task$clone()
    task$add_strata(task$target_names, bins = bins)
  }

  NextMethod("partition")
}

#' @rdname partition
#' @export
partition.TaskClassif = function(task, ratio = 0.67, stratify = TRUE, ...) { # nolint
  if (stratify) {
    task = task$clone()
    task$set_col_roles(task$target_names, add_to = "stratum")
  }

  NextMethod("partition")
}

#' @export
partition.Task = function(task, ratio = 0.67, stratify = TRUE, ...) { # nolint
  r = rsmp("holdout", ratio = ratio)$instantiate(task)
  list(train = r$train_set(1L), test = r$test_set(1L))
}

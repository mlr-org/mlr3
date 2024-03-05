hashes = function(x) {
  map_chr(unname(x), "hash")
}

phashes = function(x) {
  map_chr(unname(x), "phash")
}

#' @description
#' Calculate task hashes of resampling iterations.
#'
#' @param task ([Task]).
#' @param resampling ([Resampling]).
#' @param learner ([Learner])\cr
#' Because we only set the validation task for learners that use it, a learner can be passed here to
#' calculate the hashes accordingly, i.e. ignore the test ids when the learner does not have the
#' 'validation' property but otherwise include them.
#'
#' @return (`character()`).
#' @noRd
resampling_task_hashes = function(task, resampling, learner = NULL) {
  # validation task is set on the worker
  validation = !is.null(learner) && "validation" %in% learner$properties
  map_chr(seq_len(resampling$iters), function(i) {
    train_set = resampling$train_set(i)
    test_set = if (validation) resampling$test_set(i)
    task_hash(task, train_set, test_set)
  })
}

task_hash = function(task, use_ids, test_ids = NULL, ignore_validation_task = FALSE) {
  # order matters: we first check for test_ids and then for the validation_task
  if (!is.null(test_ids)) {
    # this does the same as task$divide(test_ids, "test")$validation_task$hash but avoids the deep clone
    prev_holdout = task$holdout_task
    task$holdout_task = NULL
    validation_task_hash = task_hash(task, use_ids = test_ids, test_ids = NULL, ignore_validation_task = TRUE)
    task$holdout_task = prev_holdout
  } else if (!ignore_validation_task) {
    validation_task_hash = task$validation_task$hash
  } else {
    validation_task_hash = NULL
  }
  calculate_hash(class(task), task$id, task$backend$hash, task$col_info, use_ids, task$col_roles,
    get_private(task)$.properties, validation_task_hash, task$holdout_task$hash)
}

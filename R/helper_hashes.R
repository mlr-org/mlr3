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
  learner_does_validation = !is.null(get0("validate", learner))
  map_chr(seq_len(resampling$iters), function(i) {
    train_set = resampling$train_set(i)
    test_set = if (learner_does_validation) resampling$test_set(i)
    task_hash(task, train_set, test_set)
  })
}

task_hash = function(task, use_ids, test_ids = NULL, ignore_internal_valid_task = FALSE) {
  # order matters: we first check for test_ids and then for the internal_valid_task
  internal_valid_task_hash = if (!is.null(test_ids)) {
    # this does the same as
    # task$internal_valid_task = test_ids
    # $internal_valid_task$hash
    # but avoids the deep clone
    task_hash(task, use_ids = test_ids, test_ids = NULL, ignore_internal_valid_task = TRUE)
  } else if (!ignore_internal_valid_task) {
    task$internal_valid_task$hash
  }

  calculate_hash(
    class(task),
    task$id,
    task$backend$hash,
    task$col_info,
    use_ids,
    task$col_roles,
    get_private(task)$.properties,
    internal_valid_task_hash,
    task$characteristics)
}
